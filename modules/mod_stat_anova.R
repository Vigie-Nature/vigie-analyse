#' manip_group_by UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_stat_anova_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Manipuler.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Test statistique"),
                    selectInput(ns("select_dataset"), label = "Utiliser les données brutes du fichier :", choices = NULL),
                    helpText("Le fichier de données brutes est souvent celui qui a servi à calculer les moyennes et non celui qui les contient."),
                    selectInput(ns("select_columns_explanatory"), label = "Vérifier si la variable explicative (X)", choices = NULL, multiple = TRUE),
                    # textOutput(ns("help_text_column")), # preciser par exemple attention si une valeur quanti
                    textOutput(ns("error_explanatory")),
                    selectInput(ns("select_column_dependant"), label = "a un effet sur la variable à expliquer (Y)", choices = NULL, multiple = FALSE), # TO DO : remove if count
                    helpText("Choisissez une variable quantitative"),
                    textOutput(ns("error_dependant")),
                    actionButton(ns("valid_tool"), label = "Valider le résultat",
                                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                    helpText("Prévisualisation du résultat"),

                    tags$div(style = 'overflow-x: scroll',
                             tableOutput(ns("dataset_preview"))
                    ),
                    plotOutput(ns("plot")),
                    htmlOutput(ns("resutat_text"))
           )
    )
  )
}

#' manip_group_by Server Functions
#'
#' @noRd
mod_stat_anova_server <- function(id, analysis_history, step_nb_react, update_stat, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         resutat_text = NULL,
                         error_text_explanatory = NULL,
                         error_text_dependant = NULL,
                         trigger = 0)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data stat : anova\n")


    observeEvent(update_stat(), {

      # populate select with datasets names

      # filter datasets only and update the select input list
      filter_and_update_datasets(analysis_history, "select_dataset", parent_session, ns)

      # populate columns with columns names
      observeEvent(input$select_dataset, {
        if (!is.null(input$select_dataset) & input$select_dataset != ""){
          cat("  update columns list\n")
          # allocate active dataset
          rv$active_dataset <- data.frame(analysis_history[[input$select_dataset]][["dataset"]])
          active_dataset_columns <- colnames(rv$active_dataset)
          updateSelectInput(session = parent_session, inputId = ns("select_columns_explanatory"), choices = active_dataset_columns)
          updateSelectInput(session = parent_session, inputId = ns("select_column_dependant"), choices = active_dataset_columns)
        }
      })

      # check if errors

      observeEvent(input$select_columns_explanatory, {
        verif_input <- rv$active_dataset  %>%
          group_by(!!sym(input$select_columns_explanatory)) %>%
          summarize(nombre = n())

        if(all(verif_input$nombre <= 1)) {
          rv$error_text_explanatory = "Le fichier ne contient qu'une seule valeur pour chaque catégorie. Il faut choisir le fichier de données brutes."
        } else if(nrow(verif_input) > 100) {
          rv$error_text_explanatory = "Le fichier contient trop de catégories différentes. Ne vouliez vous pas faire une régression linéaire ?"
        } else {
          rv$error_text_explanatory = NULL
        }
      })


      observeEvent(input$select_column_dependant,{
        column_type = class(unlist(rv$active_dataset[input$select_column_dependant]))
        if(column_type != "numeric" & column_type != "integer") {
          rv$error_text_dependant <- "Attention la colonne sur laquelle vous faites le calcul ne doit contenir que des nombres"

        } else {
          rv$error_text_dependant = NULL
        }
      })

      observeEvent(c(input$select_columns_explanatory,
                     input$select_column_dependant),{
                       if(!is.null(input$select_column_dependant) & !is.null(input$select_columns_explanatory)){
                         if (is.null(rv$error_text_dependant) & is.null(rv$error_text_explanatory) & !input$select_column_dependant == "" & !input$select_columns_explanatory == ""){

                           multiple  = length(input$select_columns_explanatory) > 1

                           # transform explanatory variable to factor
                           rv$active_dataset[ , input$select_columns_explanatory] <- as.factor(rv$active_dataset[ , input$select_columns_explanatory] )

                           if (!multiple){
                             level_names <- unique(rv$active_dataset[ , input$select_columns_explanatory])
                             if (length(level_names) > 2){
                               level_names_formated <- paste(paste(level_names[-length(level_names)], collapse = ", "), level_names[length(level_names)], sep = " et ")
                             } else {
                               level_names_formated <-paste(level_names, collapse = " et ")
                             }
                           }

                           # Build formula
                           if (multiple) {
                             model_formula <- as.formula(paste(
                               input$select_column_dependant,
                               " ~ " ,
                               paste(input$select_columns_explanatory, collapse = ' + '))
                             )

                           } else {
                             model_formula <- as.formula(paste(input$select_column_dependant," ~ " , input$select_columns_explanatory))
                           }

                           # run anova
                           res <- aov(model_formula, data = rv$active_dataset)

                           # get output from linear model
                           results <- summary(res)

                           # Prepare Graph
                           indicateur_mean_max <- rv$active_dataset %>%
                             group_by_at(input$select_columns_explanatory) %>%
                             summarise_at(vars(input$select_column_dependant), list(mean = function(x) mean(x, na.rm = TRUE),
                                                                                    max = function(x) max(x, na.rm = TRUE),
                                                                                    IC = function (x) {1.96 * sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})) %>%
                             mutate(ICmin = mean - IC,
                                    ICmax = mean + IC)

                           if (length(input$select_columns_explanatory) == 1){ # anova a un facteur et t.test a implementer
                             postHocs <- myglht(res = res, fact = input$select_columns_explanatory)
                             lettersGroupOutput <- cld(postHocs)
                             vectorLetter <- lettersGroupOutput$mcletters$Letters

                             plot <- ggplot(indicateur_mean_max, aes(x = !!sym(input$select_columns_explanatory), y = mean)) +
                               geom_point() +
                               geom_errorbar(aes(ymin = ICmin, ymax = ICmax), width = .2)+
                               theme_minimal() +
                               theme(legend.position = "none") +
                               theme(axis.text.x=element_text(angle = 35, hjust = 1))+
                               ylab(input$select_column_dependant)


                             plot <- plot + annotate("text",
                                                     x = names(vectorLetter),
                                                     y = max(indicateur_mean_max$ICmax) + .1 * (max(indicateur_mean_max$ICmax) - min(min(indicateur_mean_max$ICmin))),
                                                     label = vectorLetter)

                             output$plot <- renderPlot({
                               plot
                             })

                           }

                           letterGroup <- unique(unlist(strsplit(as.character(vectorLetter), "")))
                           rv$resutat_text = "Résultat du test statistique : <br/> <br/>"

                           if (results[[1]]$`Pr(>F)`[1] > 0.05){
                             rv$resutat_text <- paste(rv$resutat_text, "La variable ", input$select_columns_explanatory, " n'a pas d'effet significatif sur la variable ", input$select_column_dependant,". <br/> <br/>", "Les catégories ", level_names_formated, " ne sont pas significativement différentes.", sep = "")
                           } else {
                             rv$resutat_text <- paste(rv$resutat_text, "La variable ", input$select_columns_explanatory, " a un effet significatif sur la variable ", input$select_column_dependant," <br/> <br/>", sep = "")

                             for (i in 1:length(letterGroup)){
                               positionLetters = grep(pattern = letterGroup[i], vectorLetter)
                               if (length(positionLetters) > 1){
                                 rv$resutat_text <- paste(rv$resutat_text, "Les moyennes ne sont significativement pas différente pour les catégories :", paste(names(vectorLetter[positionLetters]),"<br/>", collapse = ", "))
                               } else {
                                 rv$resutat_text <- paste(rv$resutat_text, "La catégorie", names(vectorLetter[positionLetters]), "est significativement différente de toutes les autres <br/>")
                               }
                             }
                             #add warning if a variable is in more than one group
                             # get variables in 2 groups
                             variablesInTwoGroups <- which(nchar(as.character(vectorLetter))>1)
                             if (length(variablesInTwoGroups) > 1){
                               print(paste("Attention les catégories :", names(vectorLetter[variablesInTwoGroups]), "sont présentes dans plusieurs groupes"))
                             } else if (length(variablesInTwoGroups) == 1){
                               print(paste("Attention la catégorie :", names(vectorLetter[variablesInTwoGroups]), "est présente dans plusieurs groupes"))
                             }
                           }
                         }
                       }
                     })


      output$resutat_text <- renderText({
        rv$resutat_text
      })


      output$error_explanatory <- renderText({
        rv$error_text_explanatory
      })

      output$error_dependant <- renderText({
        rv$error_text_dependant
      })




      # store data
      observeEvent(input$valid_tool, {
        cat("  validate result and return from tool\n")

        if(rv$trigger < input$valid_tool) {
          rv$trigger = rv$trigger + 1

          # record values
          to_return$dataset  <- rv$tool_result
          to_return$type <- "dataset"
          to_return$type_precise <- "Manipulation de données"
          to_return$tool_name <- "Résumer les données"
          to_return$parameters <- list() # to do : add parameters for report
          to_return$parameters_text <- paste("Vous avez regroupé toutes les lignes du jeu de données :",
                                             input$select_dataset,
                                             "selon les catégories contenues dans la ou les colonnes :",
                                             paste(input$select_columns_group, collapse = " "),
                                             "en faisant la ou les opérations suivantes :",
                                             paste(input$select_operation, collapse = " "),
                                             "sur la colonne :",
                                             input$select_column_operation)

          print(to_return$parameters_text)


          # store into reactive value
          analysis_history[[paste0("Etape_", step_nb_react(), " : ", to_return$type_precise)]] <- to_return
          mod_history_server("question", analysis_history, step_nb_react())

          # go to next step UI
          updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                            selected = "navigation")
          cat("increment step_nb_react")
          step_nb_react(step_nb_react()+1)
          shinyjs::reset("valid_tool")

          #reset all
          updateSelectInput(session = parent_session, inputId = ns("select_dataset"), selected = "")
          updateSelectInput(session = parent_session, inputId = ns("select_columns_group"), selected = "")
          updateSelectInput(session = parent_session, inputId = ns("select_operation"), selected = "")
          updateSelectInput(session = parent_session, inputId = ns("select_column_operation"), selected = "")
        }
      })
    })
  })
}


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
mod_manip_row_operation_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Manipuler.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Manipulation des données"),
                    selectInput(ns("select_dataset"), label = "Faire des opération sur les colonnes du jeu de données", choices = NULL),
                    selectInput(ns("select_operation"), label = "en faisant l'opération suivante", choices = c("addition", "soustraction", "multiplication", "division", "pourcentage"), multiple = FALSE),
                    selectInput(ns("select_column_1"), label = "sur la colonne", choices = NULL, multiple = FALSE),
                    selectInput(ns("select_column_2"), label = "en fonction de la colonne", choices = NULL, multiple = FALSE),
                    textInput(ns("column_name"), label = "et donner un nouveau nom à la colonne contenant le résultat du calcul"),
                    # textOutput(ns("help_text_column")), # preciser par exemple attention si une valeur quanti
                    textOutput(ns("error")),
                    actionButton(ns("valid_tool"), label = "Valider le résultat",
                                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                    helpText("Prévisualisation du jeu de données"),

                    tags$div(style = 'overflow-x: scroll',
                             tableOutput(ns("dataset_preview"))
                    ),
                    mod_button_return_nav_ui("return_nav")
           )
    )
  )
}

#' manip_group_by Server Functions
#'
#' @noRd
mod_manip_row_operation_server <- function(id, analysis_history, step_nb_react, update_manip, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         error_text = NULL,
                         trigger = 0)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data wrangling : row_operation\n")

    observeEvent(update_manip(), {

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
          updateSelectInput(session = parent_session, inputId = ns("select_column_1"), choices = active_dataset_columns)
          updateSelectInput(session = parent_session, inputId = ns("select_column_2"), choices = active_dataset_columns)
        }
      })

      # calculate dataset
      observeEvent (c(input$select_operation,
                      input$select_column_1,
                      input$select_column_2,
                      input$column_name), {
                        if(!is.null(input$select_column_1) & !is.null(input$select_operation) & !is.null(input$select_column_2)){
                          if(input$select_column_1 != "" && input$select_column_2 != "" && input$select_operation != "") {

                            column_type_1 = class(unlist(rv$active_dataset[input$select_column_1]))
                            column_type_2 = class(unlist(rv$active_dataset[input$select_column_2]))

                            if(column_type_1 != "integer" | column_type_2 != "integer") {
                              rv$error_text <- "Attention les colonnes sur lesquelles vous faites le calcul ne doivent contenir que des nombres"

                            } else if (input$column_name == "") {
                              rv$error_text <- "Attention il faut donner un nom à votre nouvelle colonne"
                            } else {
                              cat("  calculate result for preview\n")
                              rv$tool_result <- rv$active_dataset
                              if (input$select_operation == "addition") {
                                rv$tool_result[ , input$column_name] <- rv$tool_result[ , input$select_column_1] + rv$tool_result[ , input$select_column_2]
                              } else if (input$select_operation == "soustraction") {
                                rv$tool_result[ , input$column_name] <- rv$tool_result[ , input$select_column_1] - rv$tool_result[ , input$select_column_2]
                              } else if (input$select_operation == "multiplication") {
                                rv$tool_result[ , input$column_name] <- rv$tool_result[ , input$select_column_1] * rv$tool_result[ , input$select_column_2]
                              } else if (input$select_operation == "division") {
                                rv$tool_result[ , input$column_name] <- rv$tool_result[ , input$select_column_1] / rv$tool_result[ , input$select_column_2]
                              } else if (input$select_operation == "pourcentage") {
                                rv$tool_result[ , input$column_name] <- rv$tool_result[ , input$select_column_1] / rv$tool_result[ , input$select_column_2] * 100
                              }
                              print(head(rv$tool_result))
                              rv$error_text <- NULL
                            }
                          }
                        }
                      })

      # show preview of the filter
      output$dataset_preview <- renderTable({
        head(rv$tool_result, 20)
      })

      output$error <- renderText({
        rv$error_text
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
          to_return$tool_name <- "Opération sur des lignes"
          to_return$parameters <- list() # to do : add parameters for report
          to_return$parameters_text <- paste("Vous avez fait les opérations suivantes :",
                                             paste(input$select_operation, collapse = " "),
                                             "sur les colonnes :",
                                             input$select_column_1, "et", input$select_column_2,
                                             "du jeu de données :",
                                             input$select_dataset)

          print(to_return$parameters_text)


          # store into reactive value
          analysis_history[[paste0("Etape_", step_nb_react(), " : ", to_return$type_precise)]] <- to_return
          mod_history_server("manip", analysis_history, step_nb_react())

          # go to next step UI
          updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                            selected = "navigation")
          cat("increment step_nb_react")
          step_nb_react(step_nb_react()+1)
          shinyjs::reset("valid_tool")

          #reset all
          updateSelectInput(session = parent_session, inputId = ns("select_dataset"), selected = "")
          updateSelectInput(session = parent_session, inputId = ns("select_operation"), selected = "")
          updateSelectInput(session = parent_session, inputId = ns("select_column_1"), selected = "")
          updateSelectInput(session = parent_session, inputId = ns("select_column_1"), selected = "")
        }
      })
    })
  })
}


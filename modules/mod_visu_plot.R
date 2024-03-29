#' visu_plot UI Function
#'
#' @description A shiny Module to visualise data as a graph
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param analysis_history an object that contain all the information from the analysis (datasets and information)
#' @param step_nb_react the current step number
#' @param parent_session the parent session internal parameters for {shiny}
#'
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#' @import shinyBS
mod_visu_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Visualiser.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Visualisation des données"),
                    selectInput(ns("select_dataset"), label = "Faire un graphique à partir du jeu de données", choices = NULL),
                    selectInput(ns("select_column_x"), label = "Axe des ordonnées (X)", choices = NULL),
                    selectInput(ns("select_column_y"), label = "Axe des ordonnées (Y)", choices = NULL),
                    selectInput(ns("select_type"), label = "Type de graphique", choices = c("points", "lignes", "barres", "boites de dispersion")),
                    bsCollapse(id = "advanced_option",
                               bsCollapsePanel("Options avancées",
                                               textInput(ns("x_label"), "Donner un nom personalisé à l'axe des x"),
                                               textInput(ns("y_label"), "Donner un nom personalisé à l'axe des y")
                               )),
                    actionButton(ns("valid_graph"), label = "Valider le graphique",
                                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                    helpText("Prévisualisation du graphique"),
                    tags$div(style = 'overflow-x: scroll',
                             plotOutput(ns("graph_preview"))
                    )
           )
    )
  )
}

#' visu_plot Server Functions
#'
#' @noRd
mod_visu_plot_server <- function(id, analysis_history, step_nb_react, update_visu, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         tool_result = NULL)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data visualisation : graph\n")


    observeEvent(update_visu(),{

      # populate select with datasets names
      # filter datasets only and update the select input list
      filter_and_update_datasets(analysis_history, "select_dataset", parent_session, ns)
    })

    # populate columns with columns names
    observeEvent(input$select_dataset, {
      if (!is.null(input$select_dataset) & input$select_dataset != ""){
        cat("  update columns list\n")
        # allocate active dataset
        rv$active_dataset <- analysis_history[[input$select_dataset]][["dataset"]]
        active_dataset_columns <- colnames(rv$active_dataset)
        updateSelectInput(session = parent_session, inputId = ns("select_column_x"), choices = active_dataset_columns)
        updateSelectInput(session = parent_session, inputId = ns("select_column_y"), choices = active_dataset_columns)
      }
    })



    observeEvent(c(input$select_column_x,
                   input$select_column_y,
                   input$select_type,
                   input$select_dataset,
                   input$y_label,
                   input$x_label), {
                     if(!(is.null(input$select_column_x) & is.null(input$select_column_y))) {
                       if(input$select_column_x != "" & input$select_column_y != "") {

                         rv$tool_result <- ggplot(rv$active_dataset, aes(!!sym(input$select_column_x),!!sym(input$select_column_y)))

                         if(input$select_type == "points") {
                           print("points")
                           rv$tool_result <- rv$tool_result + geom_point()
                         }

                         if(input$select_type == "boites de dispersion") {
                           print("boites de dispersion")
                           rv$tool_result <- rv$tool_result + geom_boxplot()
                         }

                         if(input$select_type == "lignes") {
                           print("lignes")
                           rv$tool_result <- rv$tool_result + geom_line()
                         }

                         if(input$select_type == "barres") {
                           print("barres")
                           rv$tool_result <- rv$tool_result + geom_col()
                         }

                         if(input$x_label != "") {
                           rv$tool_result = rv$tool_result + xlab(input$x_label)
                         }

                         if(input$y_label != "") {
                           rv$tool_result = rv$tool_result + ylab(input$y_label)
                         }
                         rv$tool_result = rv$tool_result +
                           theme_minimal() +
                           theme(axis.text.x = element_text(angle = 30, hjust = 1))

                       }
                     }
                   })


    output$graph_preview <- renderPlot({
      plot_to_show <- rv$tool_result +
        theme(axis.text = element_text(size = 20),
              axis.title = element_text(size = 25),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
      plot_to_show
    })

    # store data
    observeEvent(input$valid_graph, {
      cat("  validate result and return from tool\n")
      # record values
      to_return$graph  <- rv$tool_result
      to_return$type <- "graph"
      to_return$type_precise <- "Visualisation de données"
      to_return$tool_name <- "Faire un graphique"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$parameters_text <- paste("Vous avez fait un joli graphique")


      # store into reactive value
      analysis_history[[paste0("Etape_", step_nb_react(), " : ", to_return$type_precise)]] <- to_return
      mod_history_server("graph", analysis_history, step_nb_react())

      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "navigation")
      # increment step
      step_nb_react(step_nb_react() + 1)
    })


  })
}

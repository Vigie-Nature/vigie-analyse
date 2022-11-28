mod_history_ui <- function(id) {
  ns <- NS(id)
  tagList(
  )
}


mod_history_server <- function(id, analysis_history, step_nb_react){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # get values of current step
    history_names <- names(analysis_history)
    step_id <- as.numeric(sub(".*_", "", history_names))
    index_current_step <- which(step_id == step_nb_react)
    if (analysis_history[[history_names[index_current_step]]][["type"]] == "question"){
      insertUI(selector = "#history_reference", where = "beforeEnd",
               tagList(
                 column(width = 2, offset = 2,
                        tags$img(src = "picto/Question.png")
                 ),
                 column(width = 6,
                        tags$div(class = "left-border",
                                 h3("Etape 1 : Question de recherche"),
                                 br(),
                                 tags$p(p(analysis_history[[history_names[index_current_step]]][["question_text"]]))

                        )
                 )
               )
      )
    } else if (analysis_history[[history_names[index_current_step]]][["type"]] == "dataset"){
      insertUI(selector = "#history_reference", where = "beforeEnd",
               tagList(column(width = 2, offset = 2,
                              tags$img(src = "picto/Importer.png")
               ),
               column(width = 6,
                      tags$div(class = "left-border",
                               h3(paste("Étape", step_nb_react, ":"), "Données importées"),
                               actionButton("tabBut", "View Table"),
                               bsModal("modal_dataset", "Data Table", "tabBut", size = "large",
                                       dataTableOutput("dataset")),






                               tabsetPanel(
                                 tabPanel("Données",
                                          helpText("Voici un extrait des premières lignes du jeu de données :"),
                                          tags$div(style = 'overflow-x: scroll',
                                                   HTML(
                                                     kbl(head(analysis_history[[history_names[index_current_step]]][["dataset"]])) %>%
                                                       kable_paper() %>%
                                                       kable_styling(bootstrap_options = c("striped", "hover"))

                                                   )
                                          )

                                 ),
                                 tabPanel("Paramètres utilisés",
                                          helpText("Voici les paramètres que vous avez utilisés :"),
                                          p(analysis_history[[history_names[index_current_step]]][["parameters_text"]])
                                 )
                               )
                      )
               )
               )
      )

      output$modal_dataset <- renderDataTable({

        analysis_history[[history_names[index_current_step]]][["dataset"]]

      }, options = list(pageLength=10))
    }




  })
}



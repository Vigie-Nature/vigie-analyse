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
    step_id <- sub(".*_", "", history_names)
    step_id <- gsub(" :..*", "", x = step_id)
    step_id <- as.numeric(step_id)
    # browser()
    index_current_step <- which(step_id == step_nb_react)
    if (analysis_history[[history_names[index_current_step]]][["type"]] == "question"){
      insertUI(selector = "#history_reference", where = "beforeEnd",
               tagList(
                 column(width = 2, offset = 2,
                        tags$img(src = "picto/Question.png")
                 ),
                 column(width = 6,
                        tags$div(class = "left-border",
                                 h2("Etape 1 : Question de recherche"),
                                 br(),
                                 tags$p(p(analysis_history[[history_names[index_current_step]]][["question_text"]]))

                        )
                 )
               )
      )
    } else if (analysis_history[[history_names[index_current_step]]][["type"]] == "conclusion"){

      insertUI(selector = "#history_reference", where = "beforeEnd",
               tagList(
                 column(width = 2, offset = 2,
                        tags$img(src = "picto/Conclusion.png")
                 ),
                 column(width = 6,
                        tags$div(class = "left-border",
                                 h2("Conclusion"),
                                 br(),
                                 tags$p(p(analysis_history[[history_names[index_current_step]]][["conclusion_text"]]))

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
                               h2(paste("Étape", step_nb_react, ":"), "Données importées"),
                               tabsetPanel(
                                 tabPanel("Données",
                                          helpText("Voici un extrait des premières lignes du jeu de données :"),
                                          tags$div(style = 'overflow-x: scroll',
                                                   HTML(
                                                     # DT::datatable(analysis_history[[history_names[index_current_step]]][["dataset"]])
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



    } else if (analysis_history[[history_names[index_current_step]]][["type"]] == "graph"){
      insertUI(selector = "#history_reference", where = "beforeEnd",
               tagList(column(width = 2, offset = 2,
                              tags$img(src = "picto/Visualiserr.png")
               ),
               column(width = 6,
                      tags$div(class = "left-border",
                               h2(paste("Étape", step_nb_react, ":"), "Représentation graphique"),
                               tabsetPanel(
                                 tabPanel("Graphique",
                                           add_ggplot(analysis_history[[history_names[index_current_step]]][["graph"]], width = 7)
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



    }

  })
}



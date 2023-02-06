# This module allow the user to import data
# on validation the dataset is send to history
# User can use it repeatedly to import multiple datasets

mod_visu_choice_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Visualiser.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Etape 4 : Visualisation des données"),
                    p("Vous disposez d'outils pour représenter vos données. Cette étape vous permet de représenter clairement vos résultats et mettre en évidence les différences ou les relations entre des variables par exemple."),
                    bsCollapse(id = "collapse_visu",
                               multiple = TRUE,
                               bsCollapsePanel(title = "Graphiques",
                                               tagList(
                                                 p("Il existe deux outils pour faire des graphiques, le premier traite les tableaux avec plusieurs lignes (la plupart des utilisations)."),
                                                 h3("Représenter les données"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/histrow.gif', align = "center", width = "95%")),
                                                 br(),
                                                 img(src = "picto/Visualiser.png", height = "30px"), actionButton(ns("representer_all"), "Utiliser cet outil"),

                                                 p("Le deuxième traite les données organisée en une seule ligne, issues de Opération sur des colonnes par exemple."),
                                                 h3("Représenter un talbleau avec une seule ligne"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/histcol.gif', align = "center", width = "95%")),
                                                 br(),
                                                 img(src = "picto/Visualiser.png", height = "30px"), actionButton(ns("representer_one_row"), "Utiliser cet outil"),
                                               )
                               ),

                               bsCollapsePanel(title = "Cartes",
                                               tagList(
                                                 p("Vous pouvez aussi créer des cartes à partir de vos données. Pour cela, il faut au moins une colonne contenant des informations géographiques (académies, régions, départements, maille INPN)"),
                                                 br(),

                                                 h3("Représenter des données sur une carte"),
                                                 rep_br(2),
                                                 img(src = "picto/Visualiser.png", height = "30px"), actionButton(ns("map"), "Utiliser cet outil"),
                                               )
                               )
                    ),
                    actionButton(ns("nav_back"), "Retour au menu navigation")
           )
    )
  )


}


mod_visu_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$nav_back, {
      updateTabsetPanel(parent_session, "vigie_nature_analyse",
                        selected = "navigation")
    })

    observeEvent(input$representer_all, {
      updateTabsetPanel(parent_session, "vigie_nature_analyse",
                        selected = "tool_visu_plot")
    })



  })
}

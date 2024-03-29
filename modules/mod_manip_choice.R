# This module allow the user to import data
# on validation the dataset is send to history
# User can use it repeatedly to import multiple datasets

mod_manip_choice_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Manipuler.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Manipulation des données"),
                    p("Vous avez chargé des données, bravo. Il vous faut maintenant les manipuler, faire des calculs pour répondre à votre question de recherche."),
                    p("Vous disposez de quatre familles d'outils pour manipuler vos données. N'hésitez pas à consulter l'aide pour apprendre à utiliser les outils."),
                    bsCollapse(id = "collapse_manip",
                               multiple = TRUE,
                               bsCollapsePanel(title = "Outils pour regrouper des lignes",
                                               tagList(

                                                 h3("Résumer des données"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/resumer.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de regrouper des lignes par catégorie (par type d'environnement par exemple) en faisant un calcul (une moyenne par exemple) sur les données d'une autre colonne."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_group_by"), "Utiliser cet outil"),


                                               )
                               ),

                               bsCollapsePanel(title = "Faire des calculs sur plusieurs lignes ou plusieurs colonnes",
                                               tagList(
                                                 p("Ces outils créent une nouvelle colonne ou une nouvelle ligne qui est le résultat d'un calcul."),
                                                 br(),

                                                 h3("Opération sur des colonnes"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/operationcolonnes.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de réaliser une opération sur toutes les valeurs d'une ou plusieurs colonnes."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_operation_column"), "Utiliser cet outil"),

                                                 br(),br(),

                                                 h3("Opération sur des lignes"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/operationlignes.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de réaliser une opération sur chaque ligne d'un jeu de données."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_row_operation"), "Utiliser cet outil"),

                                               )
                               ),
                               bsCollapsePanel(title = "Trier, filtrer et masquer des colonnes",
                                               tagList(
                                                 p("Ces outils permettent de changer l'organisation des données dans les colonnes en les triant (du plus petit au plus grand par exemple), en les filtrant (en affichant qu'une seule valeur) ou en ne gardant que les colonnes jugées utiles."),
                                                 br(),

                                                 h3("Trier"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/trier.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de trier (du plus petit au plus grand par exemple) les données d'une colonne."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_operation_column"), "Utiliser cet outil"),

                                                 br(),br(),

                                                 h3("Filtrer"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/filtrer.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de ne conserver les données qu'appartenant à une catégories."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_filter"), "Utiliser cet outil"),



                                                 br(),br(),

                                                 h3("Sélectionner"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/selectionner.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de conserver uniquement les colonnes qui vous intéressent."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_operation_column"), "Utiliser cet outil"),
                                               )
                               ),


                               bsCollapsePanel(title = "Extraire de l'information à partir de colonnes",
                                               tagList(
                                                 p("Ces outils permettent de récupérer des informations à partir de chaines de caractères d'une ou plusieurs colonnes (obtenir le mois à partir d'une date complète, obtenir le département à partir d'un code postal...)."),
                                                 br(),

                                                 h3("Extraire des caractères"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/extrairecaracteres.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de récupérer des informations à partir de chaines de caractères d'une colonne."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_operation_column"), "Utiliser cet outil"),

                                                 br(),br(),

                                                 h3("Convertir des dates"),
                                                 rep_br(2),
                                                 fluidRow(img(src='gif_help/convertirdates.gif', align = "center", width = "95%")),
                                                 br(),
                                                 p("Cet outil permet de faire une opération sur des dates (obtenir le mois à partir d'une date complète)."),
                                                 img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_operation_column"), "Utiliser cet outil"),

                                               )

                               )
                    )
           )
    )
  )


}


mod_manip_choice_server <- function(id, analysis_history, step_nb_react, update_manip, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$manip_group_by, {
      updateTabsetPanel(parent_session, "vigie_nature_analyse",
                        selected = "tool_manip_group_by")
      update_manip(update_manip()+1)
    })

    observeEvent(input$manip_row_operation, {
      updateTabsetPanel(parent_session, "vigie_nature_analyse",
                        selected = "tool_manip_row_operation")
      update_manip(update_manip()+1)
    })

    observeEvent(input$manip_filter, {
      updateTabsetPanel(parent_session, "vigie_nature_analyse",
                        selected = "tool_manip_filter")
      update_manip(update_manip()+1)
    })





  })
}

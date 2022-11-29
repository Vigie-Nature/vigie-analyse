# This module allow the user to import data
# on validation the dataset is send to history
# User can use it repeatedly to import multiple datasets

mod_navigation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 6, offset = 4, align="left",
           rep_br(1),
           p("Vous pouvez continuer en important un nouveau jeu de donnée, en manipulant les données ou en les visualisant"),
           img(src = "picto/Importer.png", height = "30px"), actionButton(ns("import_nav"), "Importer d'autres données"),
           br(), br(),
           img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_nav"), "Manipuler les données"),
           br(), br(),
           img(src = "picto/Visualiser.png", height = "30px"), actionButton(ns("visu_nav"), "Visualiser les données"),
           br(), br(),
           img(src = "picto/Tester.png", height = "30px"), actionButton(ns("stat_navt"), "Effectuer un test statistique"),
           br(), br(),
           img(src = "picto/Conclusion.png", height = "30px"), actionButton(ns("conclusion_nav"), "Conclure"),
           br(), br(),
           img(src = "picto/Exporter.png", height = "30px"), actionButton(ns("rapport_nav"), "Éditer un rapport")
    )
  )
}

mod_navigation_server <- function(id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

      observeEvent(input$import_nav, {
        updateTabsetPanel(parent_session, "vigie_nature_analyse",
                          selected = "import")
      })

      observeEvent(input$manip_nav, {
        updateTabsetPanel(parent_session, "vigie_nature_analyse",
                          selected = "manip")
      })


  })
}

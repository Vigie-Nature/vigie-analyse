# This module allow the user to import data
# on validation the dataset is send to history
# User can use it repeatedly to import multiple datasets

mod_navigation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 6, offset = 4, align="left",
           rep_br(1),
           p("Vous pouvez continuer en important un nouveau jeu de donnée, en manipulant les données ou en les visualisant"),
           img(src = "picto/Importer.png", height = "30px"), actionButton(ns("import_nav"), "Importer d'autres données",
                        style = "color: #FFFFFF; background-color: #62cc33; border-color: #62cc33; font-size:120%"),
           br(), br(),
           img(src = "picto/Manipuler.png", height = "30px"), actionButton(ns("manip_nav"), "Manipuler les données",
                        style = "color: #FFFFFF; background-color: #62cc33; border-color: #62cc33; font-size:120%"),
           actionButton(ns("visu_nav"), "Visualiser les données",

                        style = "color: #FFFFFF; background-color: #62cc33; border-color: #62cc33; font-size:120%"),
           br(), br(),
           actionButton(ns("stat_navt"), "Effectuer un test statistique",
                        style = "color: #FFFFFF; background-color: #62cc33; border-color: #62cc33; font-size:120%"),
           br(), br(),
           actionButton(ns("conclusion_nav"), "Conclure",
                        style = "color: #FFFFFF; background-color: #62cc33; border-color: #62cc33; font-size:120%")
    )
  )
}

mod_navigation_server <- function(id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

      observeEvent(input$import_nav, {
        "pressed"
        updateTabsetPanel(parent_session, "vigie_nature_analyse",
                          selected = "import")
      })


  })
}

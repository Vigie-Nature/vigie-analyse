# This module allow the user to import data
# on validation the dataset is send to history
# User can use it repeatedly to import multiple datasets

mod_stat_choice_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Tester.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Etape 5 : Test statistique"),
                    h3("Choisir un test statistique"),
                    p("Pour vérifier qu'un résultat n'est pas dû au hasard, les chercheurs utilisent des tests statistiques. Quand le test statistique valide le résultat, le chercheur peut alors considérer que son hypothèse initiale est fondée."),
                    p("Nous proposons ici d'avoir une démarche similiaire, sachant qu'il ne s'agira que d'une approximation (il existe en réalité des dizaines de tests statistiques, tous adaptés à des situations bien particulières)."),
                    p("Le choix du test dépend du type de données utilisées pour la réalisation du graphique."),
                    p("Si les données en abscisses (X) sont des catégories (par exemple : un type de milieu, un numéro de département...), on fera alors une comparaison de moyennes"),
                    img(src = "picto/Tester.png", height = "30px"), actionButton(ns("map"), "Utiliser cet outil"),
                    p("Si les données en abscisses (X) sont des nombres (par exemple : un nombre d'espèces, une durée...), on fera une régression linéaire"),
                    img(src = "picto/Tester.png", height = "30px"), actionButton(ns("map"), "Utiliser cet outil"),
                    rep_br(2),
                    actionButton(ns("nav_back"), "Retour au menu navigation")
           )
    )
  )


}


mod_stat_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$nav_back, {
      updateTabsetPanel(parent_session, "vigie_nature_analyse",
                        selected = "navigation")
    })


  })
}

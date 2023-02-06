# This module allow the user to input his conclusion
# on validation the conclusion is send to history

mod_conclusion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Conclusion.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Conclusion"),
                    p("Bravo, vous êtes arrivé à la fin de votre analyse. Vous n'avez plus qu'à rédiger une conclusion pour ensuite éditer votre rapport."),
                    br(),
                    textAreaInput(ns("conclusion"), "Écrire votre conclusion", width = "90%", rows = 5, resize = "none"),
                    tags$div(class = "error",textOutput(ns("error"))),
                    br(),
                    actionButton(ns("validate_conclusion"), "Valider la conclusion"),
           )
    )
  )
}


mod_conclusion_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ReactiveValue to return
    to_return <- reactiveValues(type = NULL,
                                conclusion_text = NULL)

    # When user press validate question
    # Record values, show output, go to next step
    observeEvent(input$validate_conclusion, {

      if (input$conclusion == ""){
        output$error <- renderText("Impossible de passer à l'étape suivante : pour continuer, vous devez écrire une conclusion")

      } else{

        cat("validate conclusion\n")

        # record values
        to_return$type <- "conclusion"
        to_return$conclusion_text  <- input$conclusion
        to_return$history_title <- "Conclusion"

        # store into reactive value
        analysis_history[[paste0("step_", step_nb_react())]] <- to_return
        mod_history_server("conclusion", analysis_history, step_nb_react())

        # go to next step UI
        updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                          selected = "navigation")
        # shinyjs::reset(ns("validate_conclusion"))
        step_nb_react(step_nb_react()+1)
      }
    })
  })
}

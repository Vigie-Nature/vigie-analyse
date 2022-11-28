# This module allow the user to input his question
# on validation the question is send to history
# User can only ask one question per analysis

mod_question_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Question.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h3("Etape 1 : Question de recherche"),
                    br(),
                    textAreaInput(ns("question"), "Écrire votre question de recherche", width = "90%", rows = 5, resize = "none"),
                    tags$div(class = "error",textOutput(ns("error"))),
                    br(),
                    actionButton(ns("validate_question"), "Valider la question", style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
           )
    )
  )
}


mod_question_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ReactiveValue to return
    to_return <- reactiveValues(type = NULL,
                                question_text = NULL)

    # When user press validate question
    # Record values, show output, go to next step
    observeEvent(input$validate_question, {

      if (input$question == ""){
        output$error <- renderText("Impossible de passer à l'étape suivante : pour commencer, vous devez poser une question de recherche")

      } else{

        cat("01_validate question\n")

        # record values
        to_return$type <- "question"
        to_return$question_text  <- input$question
        to_return$history_title <- "question de recherche"

        # store into reactive value
        analysis_history[["step_1"]] <- to_return
        mod_history_server("question", analysis_history, step_nb_react())

        # go to next step UI
        updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                          selected = "import")
        # shinyjs::reset(ns("validate_question"))
        step_nb_react(step_nb_react()+1)
      }
    })
  })
}

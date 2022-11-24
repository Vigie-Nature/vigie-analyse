library(kableExtra)


server <- function(input, output, session) {
  # increase max dowload size for manual import
  options(shiny.maxRequestSize=200*1024^2)

  # define values

  # all information about the analysis will be stored here :
  # - questions
  # - datasets + parameters + comments
  # - figures + parameters + comments
  # - conclusions
  analysis_history <- reactiveValues()

  # used as parameter for a lot of functions
  # define the step number
  step_nb_react <- reactiveVal(1)

  # module to write question
  mod_question_server("question", analysis_history, step_nb_react, parent_session = session)
  mod_import_choice_server("import_choice", analysis_history, step_nb_react, parent_session = session)
  mod_navigation_server("navigation", parent_session = session)
}

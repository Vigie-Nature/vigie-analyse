# This module allow the user to input his conclusion
# on validation the conclusion is send to history

mod_button_return_nav_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns("return_nav"), "Retour au menu navigation")
  )
}

mod_button_return_nav_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # go to next step UI

    observeEvent(input$return_nav,{
    updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                      selected = "navigation")
    })
  })
}

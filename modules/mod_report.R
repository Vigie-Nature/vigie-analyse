#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 2, offset = 2,
           tags$img(src = "picto/Manipuler.png")
    ),
    column(width = 6,
           tags$div(class = "left-border",
                    h2("Manipulation des données"),
                    p("Vous avez maintenant terminé votre analyse, vous pouvez télécharger un rapport contenant toutes les étapes, depuis la questions jusqu'au test statistique"),
                    downloadButton(ns("report"), "Télécharger le rapport")
           )
    )
  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id, analysis_history){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        print("before")
        tempReport <- file.path("report.Rmd")
        print("after")

        # Set up parameters to pass to Rmd document
        params <- list(analysis_history = reactiveValuesToList(analysis_history))

        print("after again")
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        print("last after")
      }
    )
  })
}

## To be copied in the UI
# mod_report_ui("report_1")

## To be copied in the server
# mod_report_server("report_1")

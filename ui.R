#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinydashboardPlus)

source("function_utils_ui.R")
# source("modules/mod_question.R")
# source all module functions
module_folder = "modules/"
files.sources = list.files(path = module_folder)
sapply(paste0(module_folder,files.sources), source)

# Define UI for application
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # shinyWidgets::useShinydashboardPlus(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # Allow insertion of history
  div(id="history_reference"),
  rep_br(2),
  # setBackgroundColor(color = "#037971"),
  tags$head(tags$style(type = 'text/css','.navbar{display:none;}')),
  navbarPage("Vigie Nature Analyse", id = "vigie_nature_analyse",
             tabPanel("start",
                      mod_question_ui("question")
             ),
             tabPanel("import",
                      mod_import_choice_ui("import_choice")
             ),
             tabPanel("navigation",
                      mod_navigation_ui("navigation")
                      ),
             tabPanel("manip",
                      mod_manip_choice_ui("manip_choice")
             )
  ),
  rep_br(2)
)

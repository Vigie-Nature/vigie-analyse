#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinydashboardPlus)
library(blastula)

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
             ),
             tabPanel("visualisation",
                      mod_visu_choice_ui("visu_choice")
             ),
              tabPanel("stats",
                       mod_stat_choice_ui("stats_choice")
              ),
             tabPanel("conclusion",
                      mod_conclusion_ui("conclusion")
             ),
             tabPanel("tool_manip_group_by",
                      mod_manip_group_by_ui("manip_group_by")
             ),
             tabPanel("tool_visu_plot",
                      mod_visu_plot_ui("visu_plot")
             )

  ),
  rep_br(2)
)

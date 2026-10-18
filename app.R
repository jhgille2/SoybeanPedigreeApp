##################################################
## SoyBase pedigree explorer
##
## Run from the repository root with shiny::runApp(). Shiny sources
## every file in R/ before this one.
##################################################

library(shiny)
library(bslib)
library(visNetwork)
library(DT)

app_data <- load_app_data("data")

ui <- page_navbar(
  title = "Soybean pedigree explorer",
  theme = bs_theme(version = 5, preset = "flatly"),
  window_title = "Soybean pedigree explorer",
  nav_panel("Pedigree", mod_pedigree_ui("pedigree")),
  nav_panel("Coefficient of parentage", mod_cop_ui("cop")),
  nav_panel("About the data", mod_about_ui("about")),
  nav_spacer(),
  nav_item(tags$a("SoyBase", href = "https://www.soybase.org/", target = "_blank"))
)

server <- function(input, output, session) {
  mod_pedigree_server("pedigree", app_data)
  mod_cop_server("cop", app_data)
  mod_about_server("about", app_data)
}

shinyApp(ui, server)

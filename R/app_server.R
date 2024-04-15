#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- shiny::reactiveValues()
  r$yeg_neighbourhoods <- trees::yeg_neighbourhoods
  r$population <- trees::population
  r$income <- trees::income
  mod_edmonton_server("edmonton_1", r = r)
  mod_dashboard_server("dashboard_1", r = r)
  mod_mortgage_server("mortgage_1", r = r)
  mod_about_server("about_1")
  # Your application server logic
  }

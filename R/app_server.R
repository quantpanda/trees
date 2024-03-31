#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- shiny::reactiveValues()
  r$my_dataset <- trees::yeg
  mod_edmonton_server("edmonton_1", r = r)
  # Your application server logic
}

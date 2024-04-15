#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::titlePanel("About"),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::wellPanel(
                      h3("Welcome to the Edmonton Neighbourhoods Dashboard"),
                      p("This dashboard provides an overview of the neighbourhoods in Edmonton, Alberta."),
                      p("The dashboard is divided into two sections:"),
                      p("1. The map on the left shows the neighbourhoods in Edmonton. The colour of each neighbourhood is based on the number of crimes reported in that neighbourhood."),
                      p("2. The plot on the right shows the number of crimes reported in each neighbourhood."),
                      p("To get started, select a year and a crime type from the dropdown menus.")


          )
        ),
      shiny::column(12,
                    shiny::wellPanel(
                      h3("Data Sources"),
                      p("The data used in this dashboard is from the Edmonton Open Data Portal."),
                      p("The crime data is from the Edmonton Police Service and the neighbourhood boundaries data is from the City of Edmonton.")
                    )
      ),
      shiny::column(12,
                    shiny::wellPanel(
                      h3("Author: Olivier Haley"),
                      # insert link to github
                      p("You can find the code for this dashboard on my GitHub page:"),
                      a("https://github.com/tigerwoodsjr/trees")
                    )
      )

    )
  )
  }

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")

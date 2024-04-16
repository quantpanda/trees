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
                      p("This dashboard provides an overview of the neighbourhoods in Edmonton with a focus on real estate analytics."),
                      p("The dashboard is divided into three sections:"),
                      p("1. The map on the left shows the neighbourhoods in Edmonton. The colour of each neighbourhood is based on the input you select."),
                      p("2. The middle tab (Compare Neighbourhoods) is a dashboard to get familiar with potential neighborhoods."),
                      p("3. The right tab is a Mortgage Calculator which informs the user of suitable neighbourhoods within their budget.")



          )
        ),
      shiny::column(12,
                    shiny::wellPanel(
                      h3("Data Sources"),
                      p("The data used in this dashboard is from the Edmonton Open Data Portal."))
      ),
      shiny::column(12,
                    shiny::wellPanel(
                      h3("Author: Olivier Haley"),
                      # insert link to github
                      p("You can find the code for this dashboard on my GitHub page:"),
                      # link

                      shiny::a("https://github.com/tigerwoodsjr/trees")
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

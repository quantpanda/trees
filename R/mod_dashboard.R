#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_ui <- function(id, population){
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      shiny::titlePanel("Compare Neighbourhoods"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h3("Neighbourhoods Dashboard"),
          shiny::selectInput(ns("neighbourhood1"),"Select Neighbourhood 1",
            choices = base::unique(population$neighbourhood_name),
            selected = "ABBOTTSFIELD "
          ),
          shiny::selectInput(ns("neighbourhood2"),"Select Neighbourhood 2",
                             choices = base::unique(population$neighbourhood_name),
                             selected = "ABBOTTSFIELD "
          )
        ),
        shiny::mainPanel(
          plotly::plotlyOutput(ns("plot_dash"))
        )
      )
    )
  )
}

#' dashboard Server Functions
#'
#' @noRd
mod_dashboard_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_data <- reactive({
      #filter data based on input
      res <- r$population %>% dplyr::filter(r$population$neighbourhood_name == input$neighbourhood1)
      res
    })

    filtered_data2 <- reactive({
      #filter data based on input
      res2 <- r$population %>% dplyr::filter(r$population$neighbourhood_name == input$neighbourhood2)
      res2
    })

    output$plot_dash <- plotly::renderPlotly({
      #plot histogram as plotly

      plotly::plot_ly(data = filtered_data(), x = ~class, y = ~popu, name = ~input$neighbourhood1, type = 'bar',
              marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        plotly::add_trace(data = filtered_data2(), y = ~popu, name = ~input$neighbourhood2, marker = list(color = 'rgb(58,200,225)')) %>%
        plotly::layout(title = "Neighbourhoods Dashboard",
               xaxis = list(title = "Neighbourhood"),
               yaxis = list(title = "Count"),
               hovermode = "closest",
               paper_bgcolor = "rgb(243, 243, 243)",
               plot_bgcolor = "rgb(243, 243, 243)")
    })
  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard_1")

## To be copied in the server
# mod_dashboard_server("dashboard_1")

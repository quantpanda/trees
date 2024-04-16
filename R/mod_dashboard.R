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
          shiny::h3(" "),
          shiny::selectInput(ns("neighbourhood1"),"Select Neighbourhood 1",
            choices = base::unique(population$neighbourhood_name),
            selected = "ABBOTTSFIELD"
          ),
          shiny::selectInput(ns("neighbourhood2"),"Select Neighbourhood 2",
                             choices = base::unique(population$neighbourhood_name),
                             selected = "WESTMOUNT"),
          h3("Neighbourhoods Map"),
          leaflet::leafletOutput(ns("neighbourhood_map"), width = 400, height = 400),
          fixed = TRUE
        ),
        shiny::mainPanel(
          h3("Population by Age Group"),
          plotly::plotlyOutput(ns("plot_dash")),
          h3("Total Population"),
          plotly::plotlyOutput(ns("plot_dash2")),
          h3("Income Distribution"),
          plotly::plotlyOutput(ns("plot_dash3"))

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
    # filter data based on selected neighbourhoods
    filter_data <- reactive({
      r$yeg_neighbourhoods %>% dplyr::filter(neighbourhood_name %in% c(input$neighbourhood1, input$neighbourhood2))
    })


    # mark both neighbourhoods on a leaflet
    output$neighbourhood_map <- leaflet::renderLeaflet({
      # color palette
      labss <- lapply(1:nrow(filter_data()), function(i) {
        shiny::HTML(paste("Neighbourhood: ", filter_data()$neighbourhood_name[i], "<br>"))
      })
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Stadia.AlidadeSmoothDark") %>%

        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%

        leaflet::addPolygons(data = filter_data()$coords,
                             fillColor = "purple",
                             fillOpacity = 0.9,
                             color = "black",
                             opacity = 1,
                             stroke = T,
                             weight = 1,
                             label = labss,
                             highlightOptions = leaflet::highlightOptions(
                               weight = 5,
                               color = "#000",
                               bringToFront = TRUE))
    })

    output$plot_dash <- plotly::renderPlotly({
      #plot histogram as plotly
      plotly::plot_ly(data = r$population %>% dplyr::filter(neighbourhood_name == input$neighbourhood1), x = ~class, y = ~prop*100, name = ~input$neighbourhood1, type = 'bar',
              marker = list(color = 'rgb(250,207,206)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        plotly::add_trace(data = r$population %>% dplyr::filter(neighbourhood_name == input$neighbourhood2), x = ~class, y = ~prop*100, name = ~input$neighbourhood2, marker = list(color = '#23606E')) %>%
        plotly::layout(
          xaxis = list(
            zeroline = FALSE,
            title = list(
              text = "Age Group",
              font = list(
                color = "white"  # Set X axis label font color to white
              )
            ),
            tickfont = list(
              color = "white"  # Set X axis tick font color to white
            )
          ),
          yaxis = list(
            zeroline = FALSE,
            title = list(
              text = "Proportion (%)",
              font = list(
                color = "white"  # Set Y axis label font color to white
              )
            ),
            tickfont = list(
              color = "white"  # Set Y axis tick font color to white
            )
          ),
          title = list(
            text = "",
            font = list(
              color = "white"  # Set title font color to white
            )
          ),
          legend = list(
            font = list(
              color = "black"  # Set legend font color to white
            )
          ),
          paper_bgcolor = "rgb(243, 243, 243, 0)",  # Set background color to black for better contrast
          plot_bgcolor = "rgb(243, 243, 243, 0)",   # Set plot area background color to black
          showlegend = T
        )

    })
    # compare total population using horizontal bar chart
    output$plot_dash2 <- plotly::renderPlotly({

      #plot histogram as plotly
      plotly::plot_ly(data = r$yeg_neighbourhoods %>% dplyr::filter(neighbourhood_name == input$neighbourhood1), x = ~Population, y = ~neighbourhood_name, name = ~input$neighbourhood1, type = 'bar',
                      marker = list(color = 'rgb(250,207,206)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        plotly::add_trace(data = r$yeg_neighbourhoods %>% dplyr::filter(neighbourhood_name == input$neighbourhood2), y = ~neighbourhood_name, name = ~input$neighbourhood2, marker = list(color = '#23606E')) %>%
        plotly::layout(
          xaxis = list(
            zeroline = FALSE,
            title = list(
              text = "",
              font = list(
                color = "white"  # Set X axis label font color to white
              )
            ),
            tickfont = list(
              color = "white"  # Set X axis tick font color to white
            )
          ),
          yaxis = list(
            zeroline = FALSE,
            title = list(
              text = "",
              font = list(
                color = "white"  # Set Y axis label font color to white
              )
            ),
            tickfont = list(
              color = "white"  # Set Y axis tick font color to white
            )
          ),
          title = list(
            text = "",
            font = list(
              color = "white"  # Set title font color to white
            )
          ),
          legend = list(
            font = list(
              color = "black"  # Set legend font color to white
            )
          ),
          paper_bgcolor = "rgb(243, 243, 243, 0)",  # Set background color to black for better contrast
          plot_bgcolor = "rgb(243, 243, 243, 0)",   # Set plot area background color to black
          showlegend = T
        )
    })


    # Income comparison
    output$plot_dash3 <- plotly::renderPlotly({
      #plot histogram as plotly
      plotly::plot_ly(data = r$income %>% dplyr::filter(neighbourhood_name == input$neighbourhood1) %>% dplyr::arrange(class_num),x = ~class_num, y = ~prop*100, name = ~input$neighbourhood1, type = 'bar',
                      marker = list(color = 'rgb(250,207,206)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        plotly::add_trace(data = r$income %>% dplyr::filter(neighbourhood_name == input$neighbourhood2) %>% dplyr::arrange(class_num), x = ~class_num, y = ~prop*100, name = ~input$neighbourhood2, marker = list(color = '#23606E')) %>%
        plotly::layout(
          xaxis = list(
            zeroline = FALSE,

            title = list(
              text = "Income",
              font = list(
                color = "white"  # Set X axis label font color to white
              )
            ),
            tickfont = list(
              color = "white"  # Set X axis tick font color to white
            )
          ),
          yaxis = list(
            zeroline = FALSE,
            title = list(
              text = "Propotion (%)",
              font = list(
                color = "white"  # Set Y axis label font color to white
              )
            ),
            tickfont = list(
              color = "white"  # Set Y axis tick font color to white
            )
          ),
          title = list(
            text = "",
            font = list(
              color = "white"  # Set title font color to white
            )
          ),
          legend = list(
            font = list(
              color = "black"  # Set legend font color to white
            )
          ),
          paper_bgcolor = "rgb(243, 243, 243, 0)",  # Set background color to black for better contrast
          plot_bgcolor = "rgb(243, 243, 243, 0)",   # Set plot area background color to black
          showlegend = T
        )
    })
  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard_1")

## To be copied in the server
# mod_dashboard_server("dashboard_1")

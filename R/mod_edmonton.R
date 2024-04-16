#' edmonton UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_edmonton_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"), width = "100%", height = 850),
    shiny::absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left =20 , right = "auto", bottom = "auto",
                         width = 330, height = "auto", style = "background-color: rgba(0, 0, 0, 0.8); padding: 10px; border-radius: 10px;",
                        h4("Neighbourhoods"),
                        shiny::radioButtons(ns("colInput"), "Layers",
                                            choices = c("Tree density" = "tree_prop", "Number of Households" = "num_properties", "Median Property Assessment" = "median_assessed_value",
                                                        "Max Property Assessment" = "max_assessed_value"),
                                            selected = "tree_prop"),

                        br(),

                        plotly::plotlyOutput(ns("plot_dash"))
    )
  )
}


#' edmonton Server Functions
#'
#' @noRd
mod_edmonton_server <- function(id, r){
  moduleServer( id,
                function(input, output, session){
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({
      #browser()

      # # get user input for desired output
      var_x <- r$yeg_neighbourhoods[[input$colInput]]

      quantiles <- stats::quantile(var_x, probs = c(0.1, 0.25, 0.35, 0.5, 0.65, 0.75, 0.9), na.rm = TRUE)
      lower_bounds <- min(var_x, na.rm = TRUE)
      upper_bounds <- max(var_x, na.rm = TRUE)
      breaks <- c(
        lower_bounds,
        quantiles,
        upper_bounds)



      # Create color palette function
      qpal <- leaflet::colorBin("Blues", domain = var_x, bins = breaks, na.color = "grey")


      labss <- lapply(1:nrow(r$yeg_neighbourhoods), function(i) {
        shiny::HTML(paste("Neighbourhood: ", r$yeg_neighbourhoods$neighbourhood_name[i], "<br>",
                          "Tree Count: ", r$yeg_neighbourhoods$Count[i], "<br>",
                          "Tree Density: ", round(r$yeg_neighbourhoods$tree_prop[i], 2), "<br>",
                          "Median Property Assessment: ", scales::dollar(r$yeg_neighbourhoods$median_assessed_value[i], prefix = "$"), "<br>",
                          "Max Property Assessment: ", scales::dollar(r$yeg_neighbourhoods$max_assessed_value[i], prefix = "$"), "<br>",
                          "Number of Households: ", r$yeg_neighbourhoods$num_properties[i], "<br>"))
      })


      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Stadia.AlidadeSmoothDark") %>%

        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%

        leaflet::addPolygons(data = r$yeg_neighbourhoods$coords,
                             fillColor = qpal(var_x),
                             fillOpacity = 0.75,
                             color = "black",
                             opacity = 1,
                             stroke = T,
                             weight = 1,
                             label = labss,
                             highlightOptions = leaflet::highlightOptions(
                               weight = 5,
                               color = "#000",
                               bringToFront = TRUE),
                             group = "Neighbourhoods") %>%
        leaflet::addLegend(pal = qpal, values = var_x, title = "Count") %>%

        leaflet::addPolygons(data = r$parks$coords,
                             fillColor = "#038C3E",
                             fillOpacity = 0.5,
                             color = "#038C3E",
                             opacity = 1,
                             stroke = TRUE,
                             weight = 1,
                             label = r$parks$park_name,
                             group = "Parks") %>%

        leaflet::addLayersControl(
          overlayGroups = c("Neighbourhoods", "Parks"),

          options = leaflet::layersControlOptions(collapsed = FALSE, title = "Value")
        )
    })

    #Update plot on click
    observe({

      event <- input$map_shape_click

      if (is.null(event))
        return()

      # Get lat and long
      lat <- event$lat
      long <- event$lng

      # Create a data frame with the clicked point
      point_to_check <- data.frame(lat = lat, lon = long)

      # Convert the point to an sf object
      point_sf <- sf::st_as_sf(point_to_check, coords = c("lon", "lat"), crs = 4326)

      # Convert the neighborhood data frame to an sf object and set CRS
      neighborhoods_sf <- sf::st_as_sf(r$yeg_neighbourhoods$coords)
      sf::st_crs(neighborhoods_sf) <- 4326
      sf::st_crs(point_sf) <- 4326

      # Initialize a vector to store the result
      contained <- sf::st_contains(neighborhoods_sf, point_sf) %>%
        as.logical()

      containing_polygon_index <- which(contained)

      # Check if any polygon contains the point
      if (length(containing_polygon_index) > 0) {
        containing_neighborhood <- r$yeg_neighbourhoods$neighbourhood_name[containing_polygon_index]
      }

      # get the neighbourhood data

      neighbourhood_data <- r$yeg_neighbourhoods[containing_polygon_index,]

      # get the property assessment data
      property_assessment_med <- neighbourhood_data$median_assessed_value
      property_assessment_min <- neighbourhood_data$min_assessed_value
      property_assessment_max <- neighbourhood_data$max_assessed_value

      output$plot_dash <- plotly::renderPlotly({

        plotly::plot_ly() %>%
          # Add a bar chart
          # Add markers at specified x and y coordinates
          plotly::add_markers(x = c(property_assessment_min), y = c(0), name = "Min", mode = 'markers', marker = list(size = 12, color = '#23606E')) %>%
          plotly::add_markers(x = c(property_assessment_med), y = c(0), name = "Median", mode = 'markers', marker = list(size = 12, color = '#23606E')) %>%
          plotly::add_markers(x = c(property_assessment_max), y = c(0), name = "Max", mode = 'markers', marker = list(size = 12, color = '#23606E')) %>%
          # Add a line connecting the markers
          plotly::add_lines(x = c(property_assessment_min, property_assessment_med, property_assessment_max), y = c(0, 0, 0), name = " ", line = list(color = '#23606E')) %>%


          # Customize layout
          plotly::layout(
            xaxis = list(
              zeroline = FALSE,
              title = list(
                text = "",
                font = list(
                  color = "white"  # Set X axis label font color to white
                )
              ),
              showgrid = FALSE,

              tickfont = list(
                color = "white"  # Set X axis tick font color to white
              )
            ),
            yaxis = list(
              zeroline = FALSE,
              title = list(
                text = "",
                font = list(
                  color = "black"  # Set Y axis label font color to white
                )
              ),
              tickfont = list(
                color = "black"  # Set Y axis tick font color to white
              )
            ),
            title = list(
              text = paste0("Property Assessments: <br>", containing_neighborhood),
              font = list(
                color = "white"  # Set title font color to white
              )
            ),
            legend = list(
              x = 1,
              y = 0.1,
              traceorder = "normal",
              font = list(
                family = "sans-serif",
                size = 12,
                color = "white"
              ),
              bgcolor = "black",
              bordercolor = "black",
              borderwidth = 2
            ),
            paper_bgcolor = "rgb(243, 243, 243, 0)",
            plot_bgcolor = "rgb(243, 243, 243, 0)",


            showlegend = T
          )

      })
    })


  })

}

## To be copied in the UI
# mod_edmonton_ui("edmonton_1")

## To be copied in the server
# mod_edmonton_server("edmonton_1")

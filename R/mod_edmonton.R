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
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto", style = "background-color: rgba(0, 0, 0, 0.8); padding: 10px; border-radius: 10px;",
                        h3("Neighbourhoods Dashboard"),
                        shiny::radioButtons(ns("colInput"), "Layers",
                                            choices = c("Tree density" = "tree_prop", "Number of Households" = "num_properties", "Median Proprety Assessment" = "median_assessed_value",
                                                        "Max Proprety Assessment" = "max_assessed_value"),
                                            selected = "tree_prop"),

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

      # Define a larger color palette to accommodate additional breaks
      colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(length(breaks) - 1)

      # Create color palette function
      qpal <- leaflet::colorBin(colors, domain = var_x, bins = breaks, na.color = "grey")


      labss <- lapply(1:nrow(r$yeg_neighbourhoods), function(i) {
        shiny::HTML(paste("Neighbourhood: ", r$yeg_neighbourhoods$neighbourhood_name[i], "<br>",
                          "Tree Count: ", r$yeg_neighbourhoods$Count[i], "<br>",
                          "Tree Density: ", round(r$yeg_neighbourhoods$tree_prop[i], 2), "<br>",
                          "Median Property Assessment: ", r$yeg_neighbourhoods$median_assessed_value[i], "<br>",
                          "Max Property Assessment: ", r$yeg_neighbourhoods$max_assessed_value[i], "<br>",
                          "Number of Households: ", r$yeg_neighbourhoods$num_properties[i], "<br>"))
      })


      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Stadia.AlidadeSmoothDark") %>%

        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%

        leaflet::addPolygons(data = r$yeg_neighbourhoods$coords,
                             fillColor = qpal(var_x),
                             fillOpacity = 0.7,
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
        leaflet::addLayersControl(
          overlayGroups = c("Neighbourhoods"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    #Update plot on click
    observe({

      event <- input$map_shape_click
      print(event)
      str(event)
      if (is.null(event))
        return()

      # get the neighbourhood name
      neighbourhood_name <- r$yeg_neighbourhoods$neighbourhood_name[event]

      # get the neighbourhood data
      neighbourhood_data <- r$yeg_neighbourhoods[r$yeg_neighbourhoods$neighbourhood_name == neighbourhood_name, ]

      # get the property assessment data
      property_assessment_med <- neighbourhood_data$median_assessed_value
      property_assessment_min <- neighbourhood_data$min_assessed_value
      property_assessment_max <- neighbourhood_data$max_assessed_value

      output$plot_dash <- plotly::renderPlotly({
        # create the plot
        plotly::plot_ly() %>%
          plotly::add_trace(x = c("Min", "Median", "Max"),
                            y = c(property_assessment_min, property_assessment_med, property_assessment_max),
                            type = "scatter", mode = "lines+markers") %>%
          plotly::layout(title = paste("Property Assessment Range for ", neighbourhood_name),
                         xaxis = list(title = "Property Assessment"),
                         yaxis = list(title = "Value"))
      })
    })


  })

}

## To be copied in the UI
# mod_edmonton_ui("edmonton_1")

## To be copied in the server
# mod_edmonton_server("edmonton_1")

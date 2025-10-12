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
                                            choices = c("Tree Density (# of Trees Per Household)" = "tree_prop",
                                                        "Trees by Square Kilometer" = "trees_by_size",
                                                        "Tree Count" = "Count",
                                                        "Number of Households" = "num_properties",
                                                        "Median Property Assessment" = "median_assessed_value",
                                                        "Max Property Assessment" = "max_assessed_value"),
                                            selected = "tree_prop")
                        #plotly::plotlyOutput(ns("plot_dash"))
    )
  )
}


#' edmonton Server Functions
#'
#' @noRd
mod_edmonton_server <- function(id, r){
  moduleServer(id,
                function(input, output, session){
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({
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
                          "# Of Trees Per Household: ", round(r$yeg_neighbourhoods$tree_prop[i], 2), "<br>",
                          "Trees by Square Kilometer: ", round(r$yeg_neighbourhoods$trees_by_size[i], 2), "<br>",
                          "Median Property Assessment: ", scales::dollar(r$yeg_neighbourhoods$median_assessed_value[i], prefix = "$"), "<br>",
                          "Max Property Assessment: ", scales::dollar(r$yeg_neighbourhoods$max_assessed_value[i], prefix = "$"), "<br>",
                          "Number of Households: ", r$yeg_neighbourhoods$num_properties[i], "<br>"))
      })


      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldImagery") %>%

        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%

        leaflet::addPolygons(data = r$yeg_neighbourhoods$coords,
                             fillColor = qpal(var_x),
                             fillOpacity = 0.75,
                             color = "black",
                             opacity = 1,
                             stroke = T,
                             weight = 1,
                             label = labss,
                             labelOptions = leaflet::labelOptions(textsize = "20px"),
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
  })

}


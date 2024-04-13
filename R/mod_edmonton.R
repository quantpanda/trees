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
    shiny::tabPanel("Edmonton",
                    "This is the Edmonton module"
    )

  )
  leaflet::leafletOutput(ns("map"), width = "100%", height = 800)
}


#' edmonton Server Functions
#'
#' @noRd
mod_edmonton_server <- function(id, r){
  moduleServer( id,
                function(input, output, session){
    # yeg_tree_count <- trees::yeg %>%
    #   dplyr::group_by(NEIGHBOURHOOD_NAME) %>%
    #   dplyr::summarise(count = dplyr::n())

    #yeg_tree_count$colors <- grcolors[match(yeg_tree_count$`neighbourhood_name`, unique_neighbourhoods)]

    ns <- session$ns
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%

        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%
        #leaflet::addMarkers(lng = -113.4909, lat = 53.5444, popup = "Edmonton, Alberta") %>%
        # leaflet::addCircleMarkers(lng = yeg_data$LONGITUDE, lat = yeg_data$LATITUDE, radius = 5, color = yeg_data$colors, fill = TRUE, fillOpacity = 0.8,
        #                  popup = paste("Neighbourhood: ", yeg_data$NEIGHBOURHOOD_NAME, "<br>",
        #                                "Year: ", yeg_data$Year, "<br>",
        #                                "Condition: ", yeg_data$CONDITION_PERCENT, "%<br>",
        #                                "Planted: ", yeg_data$PLANTED_DATE, "<br>",
        #                                "Bears Edible Fruit: ", yeg_data$`Bears Edible Fruit`, "<br>",
        #                                "Type of Edible Fruit: ", yeg_data$`Type of Edible Fruit`, "<br>",
        #                                "Diameter: ", yeg_data$DIAMETER_BREAST_HEIGHT, "cm")) %>%
        leaflet::addPolygons(data = r$yeg_neighbourhoods, fillColor = "transparent", color = "black", weight = 1, group = "Neighbourhoods") %>%
        #leaflet::addLegend("bottomright", pal = pal, values = yeg_tree_count$count, title = "Number of Trees", labFormat = labelFormat(prefix = "", suffix = " trees"), opacity = 1) %>%

        leaflet.extras::addHeatmap(data = r$yeg, lng = ~LONGITUDE, lat = ~LATITUDE, blur = 4, max = 1, minOpacity = 1,radius = 2, group = "Trees") %>%
        leaflet::addLayersControl(
          overlayGroups = c("Neighbourhoods", "Trees"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

  })

}

## To be copied in the UI
# mod_edmonton_ui("edmonton_1")

## To be copied in the server
# mod_edmonton_server("edmonton_1")

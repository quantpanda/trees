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
    ),
    leaflet::leafletOutput(ns("map"))

  )
}

#' edmonton Server Functions
#'
#' @noRd
mod_edmonton_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%  # Set the initial view to Edmonton
        leaflet::addMarkers(lng = -113.4909, lat = 53.5444, popup = "Edmonton, Alberta")  # Add a marker for Edmonton
    })

  })
}

## To be copied in the UI
# mod_edmonton_ui("edmonton_1")

## To be copied in the server
# mod_edmonton_server("edmonton_1")

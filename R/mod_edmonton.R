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
                         width = 330, height = "auto",
                        h3("Neighbourhoods Dashboard"),

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
      var_x <- r$yeg_neighbourhoods[["Count"]]
      range_max <- max(var_x, na.rm = TRUE)
      # if range_max is zero, add a small increment to ensure uniqueness in breaks
      ifelse(range_max == 0, range_max <- 1, 0)
      # create dynamic bins
      dynamic_bins <- round(c(0, seq(10, range_max, length.out = 7)), digits = 0)
      # color palette
      pal <- leaflet::colorBin(c("Greens"), domain = var_x, bins = dynamic_bins)

      labss <- lapply(1:nrow(r$yeg_neighbourhoods), function(i) {
        shiny::HTML(paste("Neighbourhood: ", r$yeg_neighbourhoods$neighbourhood_name[i], "<br>"))
      })


      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Stadia.AlidadeSmoothDark") %>%

        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%

        leaflet::addPolygons(data = r$yeg_neighbourhoods$coords,
                             fillColor = pal(var_x),
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
        leaflet::addLegend(pal = pal, values = var_x, title = "Count") %>%

        #leaflet.extras::addHeatmap(data = r$yeg, lng = ~LONGITUDE, lat = ~LATITUDE, blur = 4, max = 1, minOpacity = 1,radius = 2, group = "Trees") %>%
        leaflet::addLayersControl(
          overlayGroups = c("Neighbourhoods"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    output$plot_dash <- plotly::renderPlotly({
      plotly::plot_ly(r$yeg_neighbourhoods, x = ~neighbourhood_name, y = ~Count, type = "bar") %>%
        plotly::layout(title = "Neighbourhoods Dashboard")
    })

  })

}

## To be copied in the UI
# mod_edmonton_ui("edmonton_1")

## To be copied in the server
# mod_edmonton_server("edmonton_1")

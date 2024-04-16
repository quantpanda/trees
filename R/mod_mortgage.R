#' mortgage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mortgage_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::titlePanel("Find Neighbourhoods Within Your Budget"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # insert text
        shiny::h4("Mortgage Calculator"),
        shiny::p("Use the mortgage calculator to determine the maximum loan amount you can afford based on your monthly payment. Note, proprety assessment values
                 cannot represent market values. The affordability is calculated based on the median assessed value of properties in each neighbourhood."),
        shiny::p("The output map will show neighbourhoods where the median assessed value is smaller than or equal to the loan amount you can afford."),
        shiny::numericInput(ns("down_payment"), "Down Payment ($)", value = 50000),
        shiny::numericInput(ns("monthly_payment"), "Monthly Payment ($)", value = 2000),
        shiny::selectInput(ns("amortization_schedule"), "Amortization Schedule",
                    choices = c("15 years", "20 years", "30 years"), selected = "30 years"),
        shiny::numericInput(ns("interest_rate"), "Interest Rate (%)", value = 4.5),
        h3("Your Budget"),
        tags$div(
          shiny::textOutput(ns("budget"), inline = TRUE),
          style = "font-size: 35px; color: #FFFFFF ; font-weight: bold;"
        )
      ),

      shiny::mainPanel(
        leaflet::leafletOutput(ns("budget_map"), width = 600, height = 600),

      )
    ))
}

#' mortgage Server Functions
#'
#' @noRd
mod_mortgage_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # filter data based on selected neighbourhoods
    filter_neighbourhood <- reactive({
      n <- ifelse(input$amortization_schedule == "15 years", 15,
                  ifelse(input$amortization_schedule == "20 years", 20, 30))

      interest_rate <- input$interest_rate / 100 / 12

      c <- input$monthly_payment

      down_payment <- input$down_payment

      # Calculate the loan amount
      loan_amount <- (c  * ((1 + interest_rate)^(n * 12)-1))/(interest_rate * ((1 + interest_rate)^(n * 12)))
      return(loan_amount+down_payment)
    })


    output$budget_map <- leaflet::renderLeaflet({
      # Calculate affordability
      affordability <- r$yeg_neighbourhoods %>%
        dplyr::filter(r$yeg_neighbourhoods$num_properties > 100) %>%
        dplyr::mutate(affordability = (filter_neighbourhood()))
      # Filter neighborhoods that user can afford
      affordable_neighborhoods <- affordability %>%
        dplyr::filter(affordability >= median_assessed_value) %>%
        dplyr::select(neighbourhood_name)

      right_neigh <- r$yeg_neighbourhoods %>% dplyr::filter(neighbourhood_name %in% c(affordable_neighborhoods$neighbourhood_name))

      # color palette
      labss <- lapply(1:nrow(right_neigh), function(i) {
        shiny::HTML(paste("Neighbourhood: ", right_neigh$neighbourhood_name[i], "<br>"),
                    paste("Median Assessed Value: ", scales::dollar(right_neigh$median_assessed_value[i], prefix = "$"), "<br>"),
                    paste("Number of Properties: ", right_neigh$num_properties[i], "<br>"))
      })
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Stadia.AlidadeSmoothDark") %>%

        leaflet::setView(lng = -113.4909, lat = 53.5444, zoom = 11) %>%

        leaflet::addPolygons(data = right_neigh$coords,
                             fillColor = "darkgreen",
                             fillOpacity = 0.7,
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


    output$budget <- renderText({
      value <- filter_neighbourhood()[[1]]
      numeric_value <- as.numeric(gsub("[^0-9.]+", "", value))
      formatted_value <- scales::dollar(numeric_value, prefix = "$")
      formatted_value
    })


  })

}


## To be copied in the UI
# mod_mortgage_ui("mortgage_1")

## To be copied in the server
# mod_mortgage_server("mortgage_1")


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
        shiny::numericInput(ns("down_payment"), "Down Payment ($)", value = 50000),
        shiny::numericInput(ns("monthly_payment"), "Monthly Payment ($)", value = 2000),
        shiny::selectInput(ns("amortization_schedule"), "Amortization Schedule",
                    choices = c("15 years", "20 years", "30 years"), selected = "30 years"),
        shiny::numericInput(ns("interest_rate"), "Interest Rate (%)", value = 4.5)
      ),

      shiny::mainPanel(
        shiny::tableOutput(ns("affordable_neighborhoods"))
      )
    ))
}

#' mortgage Server Functions
#'
#' @noRd
mod_mortgage_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$affordable_neighborhoods <- renderTable({
      # Calculate affordability based on user inputs
      n <- ifelse(input$amortization_schedule == "15 years", 15,
                  ifelse(input$amortization_schedule == "20 years", 20, 30))

      interest_rate <- input$interest_rate / 100 / 12

      c <- input$monthly_payment

      down_payment <- input$down_payment

      # Calculate the loan amount
      loan_amount <- (c  * ((1 + interest_rate)^(n * 12)-1))/(interest_rate * ((1 + interest_rate)^(n * 12)))

      # Calculate affordability
      affordability <- r$yeg_neighbourhoods %>%
        dplyr::filter(r$yeg_neighbourhoods$num_properties > 100) %>%
        dplyr::mutate(affordability = (down_payment + loan_amount))
      # Filter neighborhoods that user can afford
      affordable_neighborhoods <- affordability %>%
        dplyr::filter(affordability >= median_assessed_value) %>%
        dplyr::select(neighbourhood_name)




      return(affordable_neighborhoods)
    })
  })
}


## To be copied in the UI
# mod_mortgage_ui("mortgage_1")

## To be copied in the server
# mod_mortgage_server("mortgage_1")


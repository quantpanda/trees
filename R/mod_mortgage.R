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
  tagList(
    shiny::titlePanel("Monthly Mortgage Calculator"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput(ns("house_value"), "House Value ($)", value = 200000, min = 10000),
        shiny::numericInput(ns("down_payment"), "Down Payment ($)", value = 20000, min = 0),
        shiny::sliderInput(ns("interest_rate"), "Interest Rate (%)", value = 4, min = 0, max = 20, step = 0.1),
        shiny::sliderInput(ns("amortization_years"), "Amortization Period (Years)", value = 30, min = 1, max = 50)
      ),

      shiny::mainPanel(
        shiny::tableOutput(ns("mortgage_table"))
      )
    )

  )
}

#' mortgage Server Functions
#'
#' @noRd
mod_mortgage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$mortgage_table <- renderTable({
      # Calculate mortgage payments
      house_value <- input$house_value
      down_payment <- input$down_payment
      interest_rate <- input$interest_rate / 100 / 12
      amortization_years <- input$amortization_years * 12

      principal <- house_value - down_payment
      monthly_payment <- principal * (interest_rate * (1 + interest_rate) ^ amortization_years) / ((1 + interest_rate) ^ amortization_years - 1)

      # Generate amortization schedule
      payment_date <- seq(as.Date(Sys.Date()), by = "months", length.out = amortization_years)
      interest_payment <- principal * interest_rate
      principal_payment <- monthly_payment - interest_payment

      remaining_principal <- rep(NA, length(payment_date))
      remaining_principal[1] <- principal

      for (i in 2:length(payment_date)) {
        remaining_principal[i] <- remaining_principal[i - 1] - principal_payment
        if (remaining_principal[i] < 0) remaining_principal[i] <- 0
      }

      amortization_data <- tibble::tibble(
        Payment_Date = payment_date,
        Interest_Payment = rep(interest_payment, length(payment_date)),
        Principal_Payment = rep(principal_payment, length(payment_date)),
        Remaining_Principal = remaining_principal
      )

      amortization_data
    })

  })
}

## To be copied in the UI
# mod_mortgage_ui("mortgage_1")

## To be copied in the server
# mod_mortgage_server("mortgage_1")


#' forecasts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_forecasts_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(2,
             radioButtons(ns("dailyWeekly"), "Weekly/ daily",
                          choices = c("Daily", "Weekly")),
             radioButtons(ns("weekends"), "Include weekends?",
                          choices = c("Include weekends",
                                      "Exclude weekends"))
      ),
      column(10,
             plotOutput(ns("summaryForecast"))
      )
    )  
  )
}

#' forecasts Server Functions
#'
#' @noRd 
mod_forecasts_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    pass_data <- reactive({
      
      make_tsibble(filter_data(), frequency = input$dailyWeekly)
    })
    
    horizon <- reactive({
      
      if(input$dailyWeekly == "Daily"){
        
        return(42)
      } else {
        
        return(6)
      }
      
    })
    
    forecast <- reactive({
      
      forecast_series(pass_data(), horizon(), frequency = input$dailyWeekly)
    })
    
    output$summaryForecast <- renderPlot({
      
      plot_forecast(forecast_series = forecast(), 
                    data = pass_data(), 
                    horizon = horizon())

    })
  })
}

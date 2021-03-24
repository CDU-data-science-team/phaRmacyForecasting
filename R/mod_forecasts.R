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
             plotOutput(ns("summaryForecast")),
             
             hr(),
             
             fluidRow(
               column(6,
                      span(textOutput(ns("accuracy_text")), 
                           style = "font-size:160%"),
                      
                      hr(),
                      
                      DT::dataTableOutput(ns("accuracy"))),
               
               column(6, )
             )
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
      
      plot_forecast(forecast_value = forecast(), 
                    data = pass_data(), 
                    horizon = horizon())
      
    })
    
    output$accuracy <- DT::renderDT({
      
      show_accuracy(forecast_value = forecast(),
                    data = pass_data())
    })
    
    output$accuracy_text <- renderText(({
      
      average_use <- mean(tail(pass_data()$quantity, 
                               ifelse(input$dailyWeekly == "Daily", 
                                      365, 52)))
      
      average_use <- round(average_use, 1)

      paste0("The table below shows the ", tolower(input$dailyWeekly),
             " average error for each of the models used. 
             For comparison average ", tolower(input$dailyWeekly), 
             " issues of this drug were ", average_use,
             " in the previous year")
    }))
  })
}

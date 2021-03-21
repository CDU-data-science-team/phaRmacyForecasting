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
      
      if(input$dailyWeekly == "Daily"){
        
        filter_data() %>% 
          dplyr::filter(Total_Qty >= 0) %>% 
          dplyr::group_by(Date) %>%
          dplyr::summarise(quantity = sum(Total_Qty, na.rm = TRUE)) %>% 
          dplyr::ungroup() %>%  
          tsibble::tsibble(index = Date) %>% 
          tsibble::fill_gaps(quantity = 0)
        
      } else {
        
        filter_data() %>% 
          dplyr::filter(Total_Qty >= 0) %>% 
          dplyr::mutate(Date = floor_date(Date, "week"),
                        Date = yearweek(Date)) %>%
          dplyr::group_by(Date) %>%
          dplyr::summarise(quantity = sum(Total_Qty, na.rm = TRUE)) %>% 
          dplyr::ungroup() %>% 
          head(-1) %>% # remove the last row in case it isn't a complete week
          tsibble::tsibble(index = Date) %>% 
          tsibble::fill_gaps(quantity = 0)
      }
      
    })
    
    forecast <- reactive({
      
      drug_train <- pass_data() %>% 
        head(-42)
      
      drug_train %>% 
        fabletools::model(fable::SNAIVE(quantity ~ lag("week")), 
                          fable::ARIMA(quantity),
                          fable::ETS(quantity ~ season(method = "A")),
                          fable.prophet::prophet(quantity ~ season(7)),
                          fasster::FASSTER(quantity ~ fasster::trend(1) + 
                                             fourier(7))) %>%
        forecast::forecast(h = 42)
    })
    
    output$summaryForecast <- renderPlot({
      
      forecast() %>% 
        ggplot2::autoplot(pass_data() %>% tail(42), level = NULL)
    })
  })
}

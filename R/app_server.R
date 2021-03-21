#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  output$drugNameUI <- renderUI({
    
    drug_names <- unique(pharmacy$NSVCode)
    
    selectInput("selectDrug", "Select drug to forecast",
                choices = drug_names)
    
  })
  
  output$dateRangeUI <- renderUI({
    
    dates <- pharmacy %>% 
      dplyr::filter(NSVCode == input$selectDrug) %>% 
      dplyr::pull(Date)
    
    dateRangeInput("dateRange", "Select dates for data",
                   start = min(dates), end = max(dates),
                   min = min(dates), max = max(dates))
  })
  
  mod_forecasts_server("forecasts_ui_1")
}

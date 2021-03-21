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
  
  output$siteUI <- renderUI({
    
    sites <- sort(unique(pharmacy$Site1))
    
    selectInput("site", "Site:", choices = sites,
                selected = tail(sites, 1))
  })
  
  filter_data <- reactive({
    
    pharmacy %>% 
      dplyr::filter(NSVCode == input$selectDrug,
                    Date >= input$dateRange[1], Date <= input$dateRange[2],
                    Site1 == input$site)
    
  })
  
  mod_forecasts_server("forecasts_ui_1", filter_data = filter_data)
}

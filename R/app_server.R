#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # shiny inputs----
  
  # Settings which aren't yet provided within the code
  risk_of_min_stock <-  0.01
  risk_of_exceeding_max_stock <- 0.05
  time_til_next_order <- 10
  max_storage_capacity <-  30000
  
  # add in other waste / expiry / adjustment codes
  waste_adjust_codes <- c("ADJ","COMSP","EXP", "HADJ","HEXP", "HWAST", "MOCK", 
                          "RWAST", "TEST", "TRG", "WAST", "WASTE", "XXXX")
  
  # load the data warehouse stuff----
  
  board <- pins::board_rsconnect()
  
  product_sup_profile <- board %>% 
    pins::pin_read("Chris.Beeley/product_sup_profile")
  
  # this is supposed to load live but doesn't ATM
  
  w_order_log_df1 <- board %>% 
    pins::pin_read("Chris.Beeley/w_order_log_df1")
  
  trans_log <- board %>% 
    pins::pin_read("Chris.Beeley/trans_log")
  
  # reactive UI----
  
  output$drugNameUI <- renderUI({
    
    drug_names <- sort(unique(pharmacy$NSVCode))
    
    drug_names <- drug_names[!drug_names == "Drug D"]
    
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
  
  # data and modules----
  
  filter_data <- reactive({
    
    pharmacy %>% 
      dplyr::filter(NSVCode == input$selectDrug,
                    Date >= input$dateRange[1], Date <= input$dateRange[2],
                    Site1 == input$site)
    
  })
  
  mod_reordering_server("reordering_ui_1")
  
  mod_forecasts_server("forecasts_ui_1", filter_data = filter_data)
}

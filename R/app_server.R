#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # load the data warehouse stuff----
  
  # board <- pins::board_rsconnect()
  # 
  # product_sup_profile <- board %>% 
  #   pins::pin_read("Chris.Beeley/product_sup_profile")
  # 
  # w_requis_df1 <- board %>% 
  #   pins::pin_read("Chris.Beeley/w_requis_df1")
  
  # live load
  
  source("secret/source_shiny_live.R")
  
  # reactive UI----
  
  output$supplierUI <- renderUI({
    
    suppliers <- sort(unique(product_sup_profile$Supplier_name))
    
    selectInput("supplier", "Select supplier",
                choices = suppliers, 
                selected = "AAH")
    
  })
  
  output$siteUI <- renderUI({

    sites <- sort(unique(product_sup_profile$Site))
    
    selectInput("site", "Site:", choices = sites,
                selected = 100)
  })
  
  # data and modules----
  
  filter_data <- reactive({
    
    product_sup_profile %>% 
      dplyr::filter(Drug_code == input$selectDrug,
                    Date >= input$dateRange[1], Date <= input$dateRange[2],
                    Site == input$site)
    
  })
  
  reordering_inputs <- reactive({
    
    list(select_supplier = input$supplier, site = input$site)
  })
  
  mod_reordering_server("reordering_ui_1", 
                        react_inputs = reordering_inputs,
                        product = product_sup_profile,
                        w_order = w_order_log_df1,
                        requis = w_requis_df1)
  
  mod_forecasts_server("forecasts_ui_1", filter_data = filter_data)
}

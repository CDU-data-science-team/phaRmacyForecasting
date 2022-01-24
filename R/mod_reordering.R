#' reordering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reordering_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    DT::DTOutput(ns("reorder_table"))
    
  )
}

#' reordering Server Functions
#'
#' @noRd 
mod_reordering_server <- function(id, react_inputs, product, w_order, requis){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # define possibly function
    
    poss_inventory = purrr::possibly(.f = inventory_reorder, otherwise = NULL)
    
    output$reorder_table <- DT::renderDT({
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Loading...", value = 0)
      
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      
      req(react_inputs()$site)
      
      inventory_reorder(site = react_inputs()$site, 
                        supplier = react_inputs()$select_supplier, 
                        product, 
                        w_order, 
                        requis,
                        holidays = get_holidays(),
                        updateProgress)
      
      # table_return %>%
      # purrr::keep(~is.null(.x) )
      
    }, rownames = FALSE)
  })
}



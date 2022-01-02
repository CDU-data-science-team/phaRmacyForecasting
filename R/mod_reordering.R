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
    
    output$reorder_table <- DT::renderDT({
      
      inventory_reorder(site = 240, supplier = "STO", product, w_order, requis)
    })
  })
}



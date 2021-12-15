#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(title = "PhaRmacy foRecasting"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Reordering", tabName = "reordering", icon = icon("cubes")),
          menuItem("Forecasts", tabName = "forecasts", icon = icon("tablets")),
          menuItem("Model comparison", tabName = "compare", 
                   icon = icon("sort-amount-down")),
          menuItem("Diagnostics", tabName = "diagnostics", 
                   icon = icon("question-circle")),
          uiOutput("drugNameUI"),
          uiOutput("siteUI"),
          uiOutput("dateRangeUI")
        )
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "reordering",
                  mod_reordering_ui("reordering_ui_1")
                  
          ),
          
          # Second tab content
          tabItem(tabName = "forecasts",
                  mod_forecasts_ui("forecasts_ui_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'phaRmacyForecasting'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


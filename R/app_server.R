#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  descent_data <- reactiveValues()
  
  shiny::callModule(data_entry_page, "data_entry", descent_data)
  shiny::callModule(exploring_page, "clustering", descent_data)
  shiny::callModule(sorting_page, "sorting", descent_data)
  shiny::callModule(plotting_page, "plotting", descent_data)
}

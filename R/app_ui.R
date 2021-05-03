#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
    # Application title
    shiny::navbarPage(title = "Ontology Descent: summarizing and visualizing complex enrichment data",
                      shiny::tabPanel("Data Entry", data_entry_page_ui("data_entry")),
                      shiny::tabPanel("Clustering", exploring_page_ui("clustering")),
                     # shiny::tabPanel("Sorting", sorting_page_ui("sorting")),
                      shiny::tabPanel("Plotting", plotting_page_ui("plotting")),
                      id = "mainApp")
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
      app_title = 'OntologyDescent'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


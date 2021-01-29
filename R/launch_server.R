#' Launch the ontologyDescent shiny server
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' launch_application()
#' }
ontology_descent <- function(...)
{
  shiny::runApp(appDir = system.file("application", package = "ontologyDescent"),
                display.mode = "normal",
                ...)
}

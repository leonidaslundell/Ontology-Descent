get_test_data <- function() {
  load(here::here("R/sysdata.rda"))
  shiny::reactiveValues(inputData = example_data)
}

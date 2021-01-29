get_test_data <- function() {
  load(here::here("R/sysdata.rda"))
  shiny::reactiveValues(go_data = example_data)
}

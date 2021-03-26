get_test_data <- function() {
  example_data <-
    paste(
      example_data$ontoID,
      example_data$pValue,
      example_data$enrichmentScore,
      example_data$direction,
      sep = "\t",
      collapse = "\n"
    )
  example_data <- paste0("ontoID\tpValue\tenrichmentScore\tdirection\n", example_data)

  example_data
}

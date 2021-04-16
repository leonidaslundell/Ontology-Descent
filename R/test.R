get_test_data <- function(size = "big") {

  if(size == "big"){
    rand <- sample(1:nrow(example_data), 300, replace = F)
  } else {
    rand <- sample(1:nrow(example_data), 50, replace = F)
  }

  example_data <-
    paste(
      example_data$ontoID[rand],
      example_data$pValue[rand],
      example_data$enrichmentScore[rand],
      example_data$direction[rand],
      sep = "\t",
      collapse = "\n"
    )
  example_data <- paste0("ontoID\tpValue\tenrichmentScore\tdirection\n", example_data)

  return(example_data)
}

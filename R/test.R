get_test_data <- function(size = "big") {

  if(size == "big"){
    n_terms <- 300
  } else {
    n_terms <- 50
  }

  rand <- sample(nrow(example_data), n_terms, replace = F)

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

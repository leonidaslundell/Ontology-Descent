get_test_data <- function(size = "big") {
  if(size == "big"){
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

    return(example_data)
  }else{
    set.seed(42)
<<<<<<< HEAD
    rand <- sample(1:nrow(example_data), 50)
=======
    rand <- sample(1:length(nrow(example_data)), 50)
>>>>>>> fee0a6a71f9f1f3bb0222aef580861282bab223d
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

}

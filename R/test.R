#' Get test data
#'
#' @param size integer, number of terms to return
#'
#' @return string example_data
get_test_data <- function(size = "big") {

  if(size == "big"){
    n_terms <- 300
  } else {
    n_terms <- 50
  }

  rand <- sample(nrow(example_data), n_terms, replace = F)

  out <-
    paste(
      example_data$ontoID[rand],
      example_data$pValue[rand],
      example_data$enrichmentScore[rand],
      example_data$direction[rand],
      sep = "\t",
      collapse = "\n"
    )
  out <- paste0("ontoID\tpValue\tenrichmentScore\tdirection\n", out)

  return(out)
}

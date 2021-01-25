networkeR <- function(ont = "MF"){
  library(GO.db)
  library(igraph)
  library(data.table)
  
  #BP is very slow
  flatNet <- switch(ont,
                    MF = as.list(GOMFCHILDREN),
                    BP = as.list(GOBPCHILDREN),
                    CC = as.list(GOCCCHILDREN),
                    all = c(as.list(GOCCCHILDREN),
                            as.list(GOMFCHILDREN),
                            as.list(GOBPCHILDREN))) 
  
  flatNet <- flatNet[!is.na(flatNet)]
  flatNet <- reshape2::melt(flatNet)
  
  flatNet <- as.data.table(flatNet)
  colnames(flatNet) <- c("from", "to") 
  
  flatNet <- flatNet[,c("from", "to"):=.(as.character(from), as.character(to))]
  
  return(graph_from_data_frame(flatNet))
}
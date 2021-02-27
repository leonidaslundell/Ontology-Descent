#' Internal Functions
#'
#' @param ontoNet igraph representation of ontology hierarchy from networkeR
#' @param ontoNames ontology ID to ontology Terms
#' @param ontoLength ontology ID lengths
#' @param target target ontology IDs to cluster
#'
#' @import igraph data.table
#' @return
#' @export
#'

clustereR <- function(ontoNet, ontoNames, ontoLength, target){

  #############
  #network based clustering
  connectedSubgraph <- shortest_paths(ontoNet, from = target, to = target, mode = "all")
  connectedSubgraph <- unique(names(unlist(connectedSubgraph$vpath)))
  ontoNetSubgraph <- igraph::induced_subgraph(ontoNet, connectedSubgraph)
  ontoClust <- igraph::cluster_fast_greedy(as.undirected(ontoNetSubgraph))

  #############
  #eigen centrality quantifies connected connecteness...
  clusterTerm <- sapply(igraph::communities(ontoClust), function(x){
    xx <- igraph::induced_subgraph(ontoNet, x)
    xx <- which.max(centr_eigen(xx)$vector)
    ontoNames[x[xx]]
  })
  names(clusterTerm) <- 1:max(ontoClust$membership)

  ontoClust <- data.frame(cluster = ontoClust$membership,
                          clusterTerm = clusterTerm[membership(ontoClust)],
                          ontoID = ontoClust$names,
                          ontoTerm = GOnames[ontoClust$names],
                          ontoLength = ontoLength[ontoClust$names])

  ontoClust[ontoClust$ontoID %in% target,]
}


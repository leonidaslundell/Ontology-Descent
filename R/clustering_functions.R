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
  ontoClust <- igraph::cluster_edge_betweenness(as.undirected(ontoNetSubgraph))

  #############
  #eigen centrality quantifies connected connecteness...
  clusterTerm <- sapply(igraph::communities(ontoClust), function(x){
    xx <- igraph::induced_subgraph(ontoNet, x)
    xx <- which.max(centr_eigen(xx)$vector)
    ontoNames[x[xx]]
  })
  names(clusterTerm) <- 1:max(ontoClust$membership)

  #############
  #prepare result table

  ontoClust <- data.frame(cluster = ontoClust$membership,
                          clusterTerm = clusterTerm[membership(ontoClust)],
                          ontoID = ontoClust$names,
                          ontoTerm = GOnames[ontoClust$names],
                          ontoLength = ontoLength[ontoClust$names])

  #############
  #prepare network plot

  cols <- colorRampPalette(brewer.pal(12, "Set3"))(max(ontoClust$cluster))
  cols <- sample(cols, max(ontoClust$cluster), replace = F)
  ontoClust$color <- cols[ontoClust$cluster]

  V(ontoNetSubgraph)$color <- "black"
  V(ontoNetSubgraph)$color[match(ontoClust$ontoID, V(ontoNetSubgraph)$name)] <- cols[ontoClust$cluster]
  V(ontoNetSubgraph)$color[match(ontoClust$ontoID, V(ontoNetSubgraph)$name)] <- cols[ontoClust$cluster]
  #V(ontoNetSubgraph)$name == ontoClust$ontoID
  V(ontoNetSubgraph)$clusterTerm <- ontoClust$clusterTerm

  V(ontoNetSubgraph)$size <- 0
  V(ontoNetSubgraph)$size[V(ontoNetSubgraph)$name %in% x] <- 3

  E(ontoNetSubgraph)$arrow.size <- 0


  ontoNetSubgraph

  #remove the "stepping stones"
  ontoClust <- ontoClust[ontoClust$ontoID %in% target,]

  return(list(res = ontoClust, plot = ontoNetSubgraph))

}


networkeR <- function(ont = "MF", species = "HSA"){
  library(GO.db)
  library(igraph)
  library(data.table)

  if(!any(ont %in% c("CC", "BP", "MF","All","Reactome"))){
    stop("ont needs to be one of: CC, BP, MF, All, Reactome")
  }

  if(ont == "Reactome"){

    flatNet <- fread("https://reactome.org/download/current/ReactomePathwaysRelation.txt", header = F)
    flatNet <- flatNet[grep(species, V1)]
    colnames(flatNet) <- c("from", "to")
    flatNet

  }else{

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

    flatNet <- flatNet[,c("from", "to"):=.(as.character(from),
                                           as.character(to))]

  }

  graphNet <- graph_from_data_frame(flatNet)

  return(graphNet)
}


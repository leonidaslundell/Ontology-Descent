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
  ##commented out parts are for removing too distant connections
  # connectedSubgraph <- connectedSubgraph$vpath[sapply(connectedSubgraph$vpath, length)<8]
  connectedSubgraph <- connectedSubgraph$vpath
  connectedSubgraph <- unique(names(unlist(connectedSubgraph)))
  # connectedSubgraph <- c(connectedSubgraph, target)
  ontoNetSubgraph <- igraph::induced_subgraph(ontoNet, connectedSubgraph)
  # ontoClust <- igraph::cluster_spinglass(as.undirected(ontoNetSubgraph))
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

  cols <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(max(ontoClust$cluster))
  cols <- sample(cols, max(ontoClust$cluster), replace = F)
  cols <- cols[ontoClust$cluster]
  ontoClust$color <- cols
  ontoClust[!ontoClust$ontoID %in% target, "color"] <- "#808080"

  #cluster name

  ontoClust$nodeLabel <- ""
  ontoClust$nodeLabel[which(ontoClust$ontoTerm == ontoClust$clusterTerm)] <-
    ontoClust$clusterTerm[which(ontoClust$ontoTerm == ontoClust$clusterTerm)]

  if(!all(V(ontoNetSubgraph)$name == ontoClust$ontoID)){stop("wrong order")}

  vertex_attr(ontoNetSubgraph) <- list(name = ontoClust$ontoID,
                                       color = ontoClust$color,
                                       clusterTerm = ontoClust$clusterTerm,
                                       label.color = ontoClust$color,
                                       ontoTerm = ontoClust$ontoTerm,
                                       nodeLabel = ontoClust$nodeLabel)
  V(ontoNetSubgraph)$size <- 0
  V(ontoNetSubgraph)$size[V(ontoNetSubgraph)$name %in% target] <- 2

  E(ontoNetSubgraph)$arrow.size <- 0

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

sampleR <- function(ontoHclust,
                    maxClusterSize = 300,
                    n = 50,
                    minClusters = 75,
                    maxClusters = 1250){

  x <- NULL

  for(n in 1:n){

    #cut at a random height
    treeCut <- cutree(ontoHclust,
                      k = sample(minClusters:maxClusters,1))

    #take a random branch
    treeCutSample <- names(treeCut[treeCut == sample(unique(treeCut), 1)])

    #from a branch, take a random amount
    treeCutSample <- treeCutSample[sample(1:length(treeCutSample),
                                          sample(1:length(treeCutSample), 1))]
    x[[n]] <- treeCutSample
  }

  #this generates some gigantic trees some times since the low k numbers give you very big clusters.
  #removing clusters with more than 250 GO terms (so arbitrary)

  x[sapply(x, length)<maxClusterSize]

}



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

clustereR <- function(ontoNet, ontoNames, ontoLength, target, method = "edge_betweenness", filterTerms = NULL){

  # c("edge_betweenness",
  #   "fast_greedy",
  #   "infomap",
  #   "label_prop",
  #   "leading_eigen",
  #   "louvain",
  #   "optimal",
  #   "spinglass",
  #   "walktrap")

  #############
  #network based clustering
  connectedSubgraph <- shortest_paths(ontoNet, from = target, to = target, mode = "all")
  connectedSubgraph <- connectedSubgraph$vpath
  connectedSubgraph <- unique(names(unlist(connectedSubgraph)))
  ontoNetSubgraph <- igraph::induced_subgraph(ontoNet, connectedSubgraph)
  ontoClust <- switch(method,
                      # edge_betweenness = igraph::cluster_edge_betweenness(as.undirected(ontoNetSubgraph)),
                      fast_greedy = igraph::cluster_fast_greedy(as.undirected(ontoNetSubgraph))$membership,
                      # infomap = igraph::cluster_infomap(as.undirected(ontoNetSubgraph)),
                      # label_prop = igraph::cluster_label_prop(as.undirected(ontoNetSubgraph)),
                      leading_eigen = igraph::cluster_leading_eigen(as.undirected(ontoNetSubgraph))$membership,
                      louvain = igraph::cluster_louvain(as.undirected(ontoNetSubgraph))$membership,
                      # spinglass = igraph::cluster_spinglass(as.undirected(ontoNetSubgraph)),
                      leiden = leiden::leiden(igraph::as_adjacency_matrix(as.undirected(ontoNetSubgraph)), resolution_parameter = .5),
                      walktrap = igraph::cluster_walktrap(as.undirected(ontoNetSubgraph))$membership)

  ontoClust <- data.frame(membership = ontoClust, names = V(ontoNetSubgraph)$name)
  #############
  #eigen centrality quantifies connected connecteness...
  clusterTerm <- sapply(ontoClust$membership, function(x){

    xSub <- igraph::induced_subgraph(ontoNet, x)
    xMax <- centr_eigen(xSub)$vector
    xTerm <- ontoNames[x[which.max(xMax)]]

    if(any(xTerm %in% filterTerms)){
      xMax <- xMax[-which.max(xMax)]
      xTerm <- ontoNames[x[which.max(xMax)]]
      return(xTerm)
    }else{
      return(xTerm)
    }
  })
  names(clusterTerm) <- 1:max(ontoClust$membership)

  #############
  #prepare result table

  ontoClust <- data.frame(clusterNumber = ontoClust$membership,
                          clusterTerm = clusterTerm[membership(ontoClust)],
                          ontoID = ontoClust$names,
                          ontoTerm = GOnames[ontoClust$names],
                          ontoLength = ontoLength[ontoClust$names])

  #############
  #prepare network plot

  cols <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(max(ontoClust$clusterNumber))
  cols <- sample(cols, max(ontoClust$clusterNumber), replace = F)
  cols <- cols[ontoClust$clusterNumber]
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



#' Internal Functions
#'
#' @param ontoNet igraph representation of ontology hierarchy from networkeR
#' @param target target ontology IDs to cluster
#' @param filterTerms terms to not use as a cluster name
#' @import igraph data.table leiden
#' @return
#' @export
#'

relabelleR <- function(ontoNet,
                       target,
                       filterTerms = c("molecular_function")){

  #############
  #just return the centralest from the cluster
  connectedSubgraph <- igraph::all_shortest_paths(ontoNet, from = target, to = target, mode = "all")

  connectedSubgraph <- connectedSubgraph$res
  connectedSubgraph <- unique(names(unlist(connectedSubgraph)))
  ontoNetSubgraph <- igraph::induced_subgraph(ontoNet, connectedSubgraph)

  xMax <- igraph::centr_eigen(ontoNetSubgraph)$vector
  xTerm <- igraph::V(ontoNet)$ontoTerm[match(igraph::V(ontoNetSubgraph)$name[which.max(xMax)], igraph::V(ontoNet)$name)]

  if(any(xTerm %in% filterTerms)){
    xMax <- xMax[-which.max(xMax)]
    xTerm <- igraph::V(ontoNet)$ontoTerm[match(x[which.max(xMax)],
                                               igraph::V(ontoNet)$name)]
    return(xTerm)
  }else{
    return(xTerm)
  }
}

clustereR <- function(ontoNet,
                      target,
                      method = "louvain",
                      filterTerms = c("molecular_function")){

  #############
  #network based clustering

  connectedSubgraph <- igraph::all_shortest_paths(ontoNet, from = target, to = target, mode = "all")

  connectedSubgraph <- connectedSubgraph$res
  connectedSubgraph <- unique(names(unlist(connectedSubgraph)))
  ontoNetSubgraph <- igraph::induced_subgraph(ontoNet, connectedSubgraph)

  ontoClustCommunity <- switch(method,
                               fast_greedy = igraph::cluster_fast_greedy(igraph::as.undirected(ontoNetSubgraph))$membership,
                               leading_eigen = igraph::cluster_leading_eigen(igraph::as.undirected(ontoNetSubgraph))$membership,
                               louvain = igraph::cluster_louvain(igraph::as.undirected(ontoNetSubgraph))$membership,
                               leiden = leiden::leiden(igraph::as_adjacency_matrix(igraph::as.undirected(ontoNetSubgraph)),
                                                       resolution_parameter = .5),
                               walktrap = igraph::cluster_walktrap(igraph::as.undirected(ontoNetSubgraph))$membership)

  ontoClust <- data.frame(membership = ontoClustCommunity, names = igraph::V(ontoNetSubgraph)$name)

  #############
  #eigen centrality quantifies connected connecteness...
  clusterTerm <- sapply(unique(ontoClust$membership), function(x){

    x <- ontoClust$names[ontoClust$membership == x]

    xSub <- igraph::induced_subgraph(ontoNet, x)
    #directionality is good here...
    xMax <- igraph::centr_eigen(xSub)$vector
    xTerm <- igraph::V(ontoNet)$ontoTerm[match(x[which.max(xMax)],
                                               igraph::V(ontoNet)$name)]

    if(any(xTerm %in% filterTerms)){
      xMax <- xMax[-which.max(xMax)]
      xTerm <- igraph::V(ontoNet)$ontoTerm[match(x[which.max(xMax)],
                                                 igraph::V(ontoNet)$name)]
      return(xTerm)
    }else{
      return(xTerm)
    }
  })

  names(clusterTerm) <- unique(ontoClust$membership)

  #############
  #prepare result table

  ontoClust <- data.frame(clusterNumber = ontoClust$membership,
                          clusterTerm = clusterTerm[as.character(ontoClust$membership)],
                          ontoID = ontoClust$names,
                          ontoTerm = igraph::V(ontoNet)$ontoTerm[match(ontoClust$names,
                                                               igraph::V(ontoNet)$name)])

  #############
  #prepare network plot

  cols <- ggsci::pal_igv()(max(ontoClust$clusterNumber))
  set.seed(42)
  cols <- sample(cols, max(ontoClust$clusterNumber), replace = F)
  cols <- cols[ontoClust$clusterNumber]
  ontoClust$color <- cols
  ontoClust[!ontoClust$ontoID %in% target, "color"] <- "#808080"

  #cluster name

  ontoClust$nodeLabel <- ""
  ontoClust$nodeLabel[which(ontoClust$ontoTerm == ontoClust$clusterTerm)] <-
    ontoClust$clusterTerm[which(ontoClust$ontoTerm == ontoClust$clusterTerm)]

  if(!all(igraph::V(ontoNetSubgraph)$name == ontoClust$ontoID)){stop("wrong order")}

  igraph::vertex_attr(ontoNetSubgraph) <- list(name = ontoClust$ontoID,
                                               color = ontoClust$color,
                                               clusterTerm = ontoClust$clusterTerm,
                                               label.color = ontoClust$color,
                                               ontoTerm = ontoClust$ontoTerm,
                                               nodeLabel = ontoClust$nodeLabel)
  igraph::V(ontoNetSubgraph)$size <- 0
  igraph::V(ontoNetSubgraph)$size[igraph::V(ontoNetSubgraph)$name %in% target] <- 2

  igraph::E(ontoNetSubgraph)$arrow.size <- 0

  #remove the "stepping stones"
  ontoClust <- ontoClust[ontoClust$ontoID %in% target,]

  return(list(res = ontoClust, plot = ontoNetSubgraph, community = ontoClustCommunity))
}

networkeR <- function(ont = "MF", species = "HSA"){

  if(!any(ont %in% c("CC", "BP", "MF","All","Reactome"))){
    stop("ont needs to be one of: CC, BP, MF, All, Reactome")
  }

  if(ont == "Reactome"){

    flatNet <- data.table::fread("https://reactome.org/download/current/ReactomePathwaysRelation.txt", header = F)
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

    allNodes <- unique(c(names(flatNet), unlist(flatNet)))
    flatNet <- flatNet[!is.na(flatNet)]

    allNodes <- switch(species,
           HSA = {
             onto2eg <- as.list(org.Hs.eg.db::org.Hs.egGO2ALLEGS)
             allNodes <- allNodes[allNodes %in% names(onto2eg)]
             allNodes
           },
           MMU = {
             onto2eg <- as.list(org.Mm.eg.db::org.Mm.egGO2ALLEGS)
             allNodes <- allNodes[allNodes %in% names(onto2eg)]
             allNodes
           })

    flatNet <- flatNet[names(flatNet) %in% allNodes]
    flatNet <- reshape2::melt(flatNet)

    flatNet <- data.table::as.data.table(flatNet)
    colnames(flatNet) <- c("from", "to")

    flatNet <- flatNet[,c("from", "to"):=.(as.character(from),
                                           as.character(to))]

  }

  graphNet <- igraph::graph_from_data_frame(flatNet)

  #############
  #add the names as a graph attribute
  ontoNames <- as.list(GO.db::GOTERM)
  ontoNames <- sapply(ontoNames, Term)

  igraph::V(graphNet)$ontoTerm <- ontoNames[igraph::V(graphNet)$name]

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



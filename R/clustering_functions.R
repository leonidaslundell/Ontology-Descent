#' clustereR
#'
#' @param ontoNet igraph representation of ontology hierarchy from networkeR
#' @param target target ontology IDs to cluster
#' @param method clustering method (leiden default)
#' @param filterTerms terms to not use as a cluster name
#' @param forceCluster ontological IDs to force into separate cluster
#' @param seed set seed for reproducibility
#' @param simplify collapse intermediate ontology terms into a cluster
#' @import igraph data.table leidenAlg
#' @return
#' @export

clustereR <- function(ontoNet,
                      target,
                      method = "leiden",
                      filterTerms = c("molecular_function", "intracellular non-membrane-bounded organelle"),
                      forceCluster = NULL,
                      simplify = F,
                      seed = 42){

  if(!all(target %in% V(ontoNet)$name)){
    target <- target[!target %in% V(ontoNet)$name]
    return(paste0("These ontology IDs were not found in the provided ontology network: ", target))
  }

  #############
  #network based clustering

  connectedSubgraph <- igraph::all_shortest_paths(ontoNet, from = target, to = target, mode = "all")

  connectedSubgraph <- connectedSubgraph$res
  connectedSubgraph <- unique(names(unlist(connectedSubgraph)))

  while(!all(target %in% connectedSubgraph)){
    missingSubgraph <- igraph::all_shortest_paths(ontoNet,
                                                  from = target[!target %in% connectedSubgraph],
                                                  to = target[!target %in% connectedSubgraph],
                                                  mode = "all")

    missingSubgraph <- missingSubgraph$res
    missingSubgraph <- unique(names(unlist(missingSubgraph)))

    connectedSubgraph <- c(missingSubgraph,
                           connectedSubgraph)
  }

  ontoNetSubgraph <- igraph::induced_subgraph(ontoNet, connectedSubgraph)

  ontoClustCommunity <- switch(method,
                               fast_greedy = igraph::cluster_fast_greedy(igraph::as.undirected(ontoNetSubgraph))$membership,
                               leading_eigen = igraph::cluster_leading_eigen(igraph::as.undirected(ontoNetSubgraph))$membership,
                               louvain = igraph::cluster_louvain(igraph::as.undirected(ontoNetSubgraph))$membership,
                               leiden = {
                                 set.seed(seed)
                                 leidenAlg::find_partition(igraph::as.undirected(ontoNetSubgraph),
                                                           edge_weights = rep(1, ecount(ontoNetSubgraph)),
                                                           resolution = .6) +1
                               },
                               walktrap = igraph::cluster_walktrap(igraph::as.undirected(ontoNetSubgraph))$membership)

  ontoClust <- data.table::data.table(membership = ontoClustCommunity, names = igraph::V(ontoNetSubgraph)$name)

  #create a new cluster with all the nodes in forceCluster
  if(!is.null(forceCluster)){
    membershipMax <- max(ontoClust$membership) + 1
    ontoClust[names %in% forceCluster, membership:=membershipMax]
  }

  #############
  #eigen centrality quantifies connected connecteness...
  clusterTerm <- sapply(unique(ontoClust$membership), function(x){

    x <- ontoClust$names[ontoClust$membership == x]

    xSub <- igraph::induced_subgraph(ontoNet, x)
    #directionality is good here...
    set.seed(seed)
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

  set.seed(seed)
  cols <- ggsci::pal_igv()(max(ontoClust$clusterNumber))
  set.seed(seed)
  cols <- sample(cols, max(ontoClust$clusterNumber), replace = F)
  cols <- cols[ontoClust$clusterNumber]
  ontoClust$color <- cols
  ontoClust[!ontoClust$ontoID %in% target, "color"] <- "#808080"

  #cluster name (instead of targeting a specific node, try to make a 2D space of nodes, and put the label in the center of that)

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

  ontoNetSubgraph <- as.undirected(ontoNetSubgraph)

  #collapse stepping stones into one big cluster

  if(simplify){
    x <- leidenAlg::find_partition(as.undirected(ontoNetSubgraph),
                                   edge_weights = rep(1, ecount(ontoNetSubgraph))) + 1
    x <- list(membership = x, names = V(ontoNetSubgraph)$name)

    x$membership[x$names %in% V(ontoNetSubgraph)$name[V(ontoNetSubgraph)$size>0]] <-
      (max(x$membership) + 1):(sum(V(ontoNetSubgraph)$size>0) + max(x$membership))

    ontoNetSubgraph <- contract.vertices(ontoNetSubgraph,
                                    x$membership,
                                    vertex.attr.comb = list(name = function(x) paste0(x, collapse = "_"),
                                                            size = function(x) length(x),
                                                            color = function(x) unique(x),
                                                            clusterTerm = function(x) unique(x),
                                                            label.color = function(x) unique(x),
                                                            ontoTerm = function(x) paste0(x, collapse = " & "),
                                                            nodeLabel = function(x) x[x != ""]))

    #make the size proportional to the number of steping stones
    V(ontoNetSubgraph)$size[V(ontoNetSubgraph)$size>1] <- V(ontoNetSubgraph)$size[V(ontoNetSubgraph)$size>1]/max(V(ontoNetSubgraph)$size[V(ontoNetSubgraph)$size>1]) * 5

    ontoNetSubgraph <- simplify(ontoNetSubgraph, remove.loops = T, remove.multiple = T)

  }

  # if(all(grepl("R-", igraph::V(ontoNetSubgraph)$name, fixed = T))){
  #   set.seed(seed)
  #   ontoNetSubgraph <- igraph::add_layout_(ontoNetSubgraph, igraph::with_fr())
  # }else{
  #   set.seed(seed)
  #   ontoNetSubgraph <- igraph::add_layout_(ontoNetSubgraph, igraph::nicely(), igraph::component_wise())
  # }

  #make the layout "permament"
  set.seed(seed)
  ontoNetSubgraph <- igraph::add_layout_(ontoNetSubgraph, igraph::nicely(), igraph::component_wise())

  #remove the "stepping stones"
  ontoClust <- ontoClust[ontoClust$ontoID %in% target,]

  return(list(res = ontoClust, plot = ontoNetSubgraph, community = ontoClustCommunity))
}


#' networkeR
#'
#' @param ont Select ontology type to generate a network. One of c("CC", "BP", "MF","All","Reactome")
#' @param species Select species to filter our terms without any genes.
#' @import igraph data.table
#' @return ontoNet
#' @export

networkeR <- function(ont = "MF", species = "HSA"){

  if(!any(ont %in% c("CC", "BP", "MF","All","Reactome"))){
    stop("ont needs to be one of: CC, BP, MF, All, Reactome")
  }

  if(ont == "Reactome"){

    flatNet <- data.table::fread("https://reactome.org/download/current/ReactomePathwaysRelation.txt", header = F)
    flatNet <- flatNet[grep(species, V1)]
    colnames(flatNet) <- c("from", "to")

  }else{

    flatNet <- switch(ont,
                      MF = as.list(GO.db::GOMFCHILDREN),
                      BP = as.list(GO.db::GOBPCHILDREN),
                      CC = as.list(GO.db::GOCCCHILDREN),
                      All = c(as.list(GO.db::GOCCCHILDREN),
                              as.list(GO.db::GOMFCHILDREN),
                              as.list(GO.db::GOBPCHILDREN)))

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
  if(ont != "Reactome"){
    ontoNames <- as.list(GO.db::GOTERM)
    ontoNames <- sapply(ontoNames, Term)

    igraph::V(graphNet)$ontoTerm <- ontoNames[igraph::V(graphNet)$name]
  }else{
    ontoNames <- data.table::fread("https://reactome.org/download/current/ReactomePathways.txt", header = F)
    ontoNames <- ontoNames[grep(species, V1)]
    ontoNames <- structure(ontoNames$V2, names = ontoNames$V1)

    igraph::V(graphNet)$ontoTerm <- ontoNames[igraph::V(graphNet)$name]
  }

  return(graphNet)
}


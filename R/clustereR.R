#clustering in two flavors: hierachichal and topological (kmeans or igraph cluster optimal)

clustereR <- function(ontoNet, ontoNames, ontoLength, target){

  #############
  #network based clustering
  ontoNetSubgraph <- induced_subgraph(ontoNet, target)
  # ontoClust <- cluster_walktrap(ontoNetSubgraph)
  # ontoClust <- cluster_infomap(as.undirected(ontoNetSubgraph))
  ontoClust <- cluster_fast_greedy(as.undirected(ontoNetSubgraph))
  # table(ontoClust$membership)

  ontoClust <- data.frame(cluster = ontoClust$membership,
                          ontoID = ontoClust$names,
                          ontoTerm = GOnames[ontoClust$names],
                          ontoLength = ontoLength[ontoClust$names])

  x <- as.data.table(ontoClust)[,
                                .SD$ontoTerm[which.max(.SD$ontoLength)],
                                by = "cluster"]
  ontoClust <- merge(ontoClust, x, by = "cluster")
  colnames(ontoClust)[5] <- "clusterTerm"

  # #############
  # #make a dendrogram for the order
  # #some stupid bug here...
  # x <- distances(ontoNet,
  #                v = ontoClust$ontoID[ontoClust$ontoTerm %in% ontoClust$clusterTerm],
  #                to = ontoClust$ontoID[ontoClust$ontoTerm %in% ontoClust$clusterTerm],
  #                mode = "all")
  # x[is.infinite(x)] <- max(x[!is.infinite(x)])*5
  # ontoOrder <- hclust(as.dist(x))
  #
  # ontoClust$clusterID <- ontoClust$ontoID[match(ontoClust$clusterTerm, ontoClust$ontoTerm)]
  #
  # ontoClust$clusterOrder <- ontoOrder$order[match(ontoClust$clusterID, ontoOrder$labels)]

  ontoClust
}


# clustereR <- function(ontoGraph, ontoDist, onto2genes, nClusters = NULL){
#
#   ontoClust <- hclust(as.dist(ontoDist))
#
#   #cut the tree at each leaf position of the tree
#   ontoClustGroups <- lapply(rev(unique(ontoClust$height)), function(hClust){
#     goClustMemb <- cutree(ontoClust, h = hClust)
#
#     goClustMemb <- lapply(unique(goClustMemb), function(i){
#       names(goClustMemb[goClustMemb == i])
#     })
#     goClustMemb
#   })
#
#   groupsPerHeight <- sapply(ontoClustGroups, length)
#
#   if(!is.null(nClusters)){
#     return(ontoClustGroups[[which(groupsPerHeight == nClusters)]])
#   }
#
#
#   clusterPerHeight <- reshape2::melt(ontoClustGroups)
#   colnames(clusterPerHeight) <- c("ontoID", "cluster", "height")
#
#   #subset each cluster from the different heights, and get all the edges.
#   ontoClustTree <- lapply(ontoClustGroups, function(hClust){
#
#     cluster <- lapply(hClust, function(cluster){
#       as_data_frame(induced_subgraph(net, cluster))
#     })
#     cluster[sapply(cluster, function(x) nrow(x)>0)]
#     # cluster
#   })
#
#
#   #get the jackard index for the genes in each edge
#   jackIndex <- function(x, y){
#     length(intersect(x,y))/length(unique(c(x,y)))
#   }
#
#   ontoClustJack <- lapply(ontoClustTree, function(hClust){
#     sapply(hClust, function(cluster){
#       if(nrow(cluster)>0){
#         apply(cluster, 1, function(x){
#           jackIndex(onto2genes[[x[1]]],
#                     onto2genes[[x[2]]])
#         })
#       }else{
#         0
#       }
#     })
#   })
#
#   #get the 1st quantile max, and the 3rd quantile min for each height of the treeCut
#
#   param <- sapply(ontoClustJack, function(hClust){
#     quant <- sapply(hClust, quantile, probs = seq(0, 1, 0.2))
#     l <- sapply(hClust, length)
#
#     med <- sapply(hClust, median)
#
#     c(maxMin = max(quant["20%",l>1]),
#       minMax = min(quant["80%",l>1]))
#
#   })
#
#   param <- as.data.frame(t(param))
#   param$height <- rev(unique(ontoClust$height))
#   param$clusterSize <- groupsPerHeight
#
#   return(list(cluster = param, dendro = ontoClust))
# }

#clustering in two flavors: hierachichal and topological (kmeans or igraph cluster optimal)

clustereR <- function(ontoGraph, ontoDist, go2genes, nClusters = NULL){

  ontoClust <- hclust(as.dist(ontoDist))

  #cut the tree at each leaf position of the tree
  ontoClustGroups <- lapply(rev(unique(ontoClust$height)), function(hClust){
    goClustMemb <- cutree(ontoClust, h = hClust)

    goClustMemb <- lapply(unique(goClustMemb), function(i){
      names(goClustMemb[goClustMemb == i])
    })
    goClustMemb
  })

  groupsPerHeight <- sapply(ontoClustGroups, length)

  if(!is.null(nClusters)){
    return(ontoClustGroups[[which(groupsPerHeight == nClusters)]])
  }


  clusterPerHeight <- reshape2::melt(ontoClustGroups)
  colnames(clusterPerHeight) <- c("ontoID", "cluster", "height")

  #subset each cluster from the different heights, and get all the edges.
  ontoClustTree <- lapply(ontoClustGroups, function(hClust){

    cluster <- lapply(hClust, function(cluster){
      as_data_frame(induced_subgraph(net, cluster))
    })
    cluster[sapply(cluster, function(x) nrow(x)>0)]
    # cluster
  })


  #get the jackard index for the genes in each edge
  jackIndex <- function(x, y){
    length(intersect(x,y))/length(unique(c(x,y)))
  }

  ontoClustJack <- lapply(ontoClustTree, function(hClust){
    sapply(hClust, function(cluster){
      if(nrow(cluster)>0){
        apply(cluster, 1, function(x){
          jackIndex(go2genes[[x[1]]],
                    go2genes[[x[2]]])
        })
      }else{
        0
      }
    })
  })

  #get the 1st quantile max, and the 3rd quantile min for each height of the treeCut

  param <- sapply(ontoClustJack, function(hClust){
    quant <- sapply(hClust, quantile)
    l <- sapply(hClust, length)

    med <- sapply(hClust, median)

    c(maxMin = max(quant["25%",l>1]),
      minMax = min(quant["75%",l>1]))

  })

  param <- as.data.frame(t(param))
  param$height <- rev(unique(ontoClust$height))
  param$clusterSize <- groupsPerHeight

  return(list(cluster = param, dendro = ontoClust))
}

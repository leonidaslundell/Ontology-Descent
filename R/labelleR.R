labelleR <- function(ontoGraph = NULL,
                     onto2genes = NULL,
                     onto2length = NULL,
                     onto2names,
                     method,
                     ontoClust){
  #function to implement various methods for labelling.
  #3 initial options:
  #graph centrality based.
  #jackard index
  #longest.
  #longest is quickest to implement

  sizeR <- function(ontoClust, onto2length){
    names(ontoClust) <- sapply(ontoClust, function(clust){
      clust[which.max(onto2length[clust])]
    })
    return(ontoClust)
  }

  grapheR <- function(ontoClust, ontoGraph){
    names(ontoClust) <- sapply(ontoClust, function(clust){
      x <- induced_subgraph(ontoGraph, clust)
      clust[which.max(centralization.degree(x)$res)]
    })
    return(ontoClust)
  }

  #run the selected labelling method.
  #output will always be named list
  out <- switch(method,
                "size" = sizeR(ontoClust, onto2length),
                "graph" = grapheR(ontoClust, ontoGraph))

  #melt this
  out <- reshape2::melt(out)
  colnames(out) <- c("ontoIDinput", "ontoIDcluster")

  out$inputName <- onto2names[out$ontoIDinput]
  out$clusterName <- onto2names[out$ontoIDcluster]

  out
}

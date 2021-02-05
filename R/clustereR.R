#clustering in two flavors: hierachichal and topological (kmeans or igraph cluster optimal)

clustereR <- function(ontoGraph, ontoDist, go2genes, plot = T){

  ontoClust <- hclust(as.dist(ontoDist), "ward.D2")
  # plot(ontoClust, labels = F)

  #cut the tree at each leaf position of the tree
  ontoClustGroups <- lapply(rev(unique(ontoClust$height)), function(hClust){
    goClustMemb <- cutree(ontoClust, h = hClust)

    goClustMemb <- lapply(unique(goClustMemb), function(i){
      names(goClustMemb[goClustMemb == i])
    })
    goClustMemb
  })

  groupsPerHeight <- sapply(ontoClustGroups, length)
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
  return(param)

  # par(mfcol = c(1,2),
  #     par(mar = c(3.2,2,2,2)))
  # #dendrogram
  # plot(ontoClust,
  #      labels = F,
  #      main = "GO term hierarchichal\nontology clustering",
  #      xlab = "",
  #      axes = F,
  #      ylim = c(0,12))
  # axis(side = 2, labels = 0:max(ontoClust$height), at = 0:max(ontoClust$height))
  # abline(h = rev(unique(ontoClust$height))+0.3, lty = "dashed", col = "gray")
  # text(x = max(ontoClust$merge),
  #      y = rev(unique(ontoClust$height))+0.3,
  #      labels = sapply(ontoClustGroups, length), cex = .8, col = "darkred")
  #
  # par(mar = c(4,2,2,2))
  # #metrics
  # plot(x = param$maxMin, y = 1:length(unique(ontoClust$height)),
  #      axes = F,
  #      ylim = c(0, max(ontoClust$height)),
  #      xlab = "Jackard Index", ylab = "", type = "b", pch = 16,
  #      xlim = c(min(out), max(out)), col = "darkred",
  #      main = "Minimum and maximum\ninformation per cluster")
  # lines(x = out[2,], y = 1:length(unique(ontoClust$height)), type = "b", pch = 16, col = "darkblue")
  # axis(side = 2, labels = 0:max(ontoClust$height), at = 0:max(ontoClust$height))
  # axis(side = 1,
  #      labels = round(seq(from = min(out), to = max(out), length.out = 5), 1),
  #      at = seq(from = min(out), to = max(out), length.out = 5))
  # legend(x = min(out) * 1.1,
  #        y = 0.5,
  #        legend = c("maxMin", "minMax"),
  #        fill = c("darkred", "darkblue"),
  #        bty = "n",
  #        cex = .6,
  #        ncol = 2)

  # plotDendro <-  ggdendro::ggdendrogram(goClust, labels = "F", theme_dendro = F) +
  #   scale_y_continuous(breaks = 1:max(goClust$height), expand = c(.0,0.5), position = "right") +
  #   geom_text(mapping = aes(x = max(goClust$merge)*1.1,
  #                           y = rev(unique(goClust$height))+0.3,
  #                           label = sapply(goClustGroups, length))) +
  #   geom_hline(yintercept = rev(unique(goClust$height))+0.3, lty = "dashed", col = "gray") +
  #   theme(axis.text.y = element_text(size = 15, vjust = .4, angle = 0),
  #         axis.text.x = element_blank(),
  #         axis.line.y = element_line(),
  #         axis.ticks.y = element_line(),
  #         axis.ticks.x = element_blank(),
  #         axis.title = element_blank(),
  #         plot.margin = margin(0, 0, 0, 0, "cm"),
  #         panel.background = element_rect(fill = NA)) +
  #   ggtitle("GO term clustering")
  #
  # plotMetrics <- ggplot(outPlotMetrics, aes(x = treeHeight, y = Jackard, col = variable)) +
  #   geom_line() +
  #   geom_point() +
  #   scale_color_discrete(name = "") +
  #   scale_x_continuous(breaks = 1:max(goClust$height), limits = c(0,max(goClust$height))) +
  #   scale_y_continuous(position = "right") +
  #   coord_flip() +
  #   xlab("")  + ylab("Jackard index") +
  #   theme(panel.background = element_rect(fill = NA),
  #         axis.text.y = element_blank(),
  #         axis.line = element_line(),
  #         plot.margin = margin(0, 0, 0, 0, "cm")) +
  #   ggtitle("Cluster homogeneity \nat different heights")
  #
  # print(plotDendro + plotMetrics)

  return(ontoClust)
}

#clustering in two flavors: hierachichal and topological (kmeans or igraph cluster optimal)

#this should output a plot that informs the user of the various clusters
#and also the clusters = for speed

plotereR <- function(ontoClust){

  ggplot(ontoClust$cluster, aes(x = height)) +
    geom_line(aes(y = maxMin), color = "red") +
    geom_line(aes(y = minMax), color = "black") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.25), labels = seq(0,1,.25)) +
    scale_x_continuous(limits = c(0,max(ontoClust$cluster$height) + 1),
                       breaks = ontoClust$cluster$height, expand = c(0,0)) +
    geom_vline(xintercept = ontoClust$cluster$height, color = "darkgray", lty = "dotted") +
    coord_flip() +
    theme(panel.border = element_rect(fill = NA),
          panel.background = element_blank()) +
    ylab("") +

    ggdendrogram(data = ontoClust$dendro) +
    scale_y_continuous(limits = c(0, max(ontoClust$cluster$height) + 1),
                       breaks = ontoClust$cluster$height,
                       labels = ontoClust$cluster$clusterSize,
                       position = "right", expand = c(0,0)) +
    geom_hline(yintercept = ontoClust$cluster$height, color = "darkgray", lty = "dotted") +
    ylab("Number of clusters") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(angle = 0, color = "black"),
          axis.ticks.y = element_line(),
          axis.title.y = element_text(angle = 0),
          plot.margin = unit(c(0,0,0,0), "mm"),
          panel.border = element_rect(fill = NA))



}

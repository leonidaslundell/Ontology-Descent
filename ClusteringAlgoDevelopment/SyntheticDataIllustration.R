library(dendextend)
library(magrittr)

pdf("ClusteringAlgoDevelopment/figures/schematic.benchmark.pdf", height = 5, width = 5 )

set.seed(42)
g <- make_tree(20, 8)
V(g)$size <- 10
V(g)$size[which.max(centr_clo(g)$res)] <- 25
V(g)$vertex.color <- "blue"
V(g)$vertex.color[order(centr_clo(g)$res, decreasing = T)[1:3]] <- "red"

plot(g,
     vertex.label = "",
     vertex.label.color = "black",
     vertex.color = V(g)$vertex.color,
     edge.arrow.size = .5,
     main = "Ontological relationship")

dend <- USArrests %>%  scale %>%
  dist %>% hclust %>% as.dendrogram
dendOrder <- USArrests %>%  scale %>%
  dist %>% hclust

dendCut <- cutree(dendOrder, k = 2)
dendCut <- sample(names(dendCut[dendCut == 1]), size = 9, replace = F)

dendCut <- dendOrder$labels[dendOrder$order] %in% dendCut
dendCut <- as.numeric(dendCut) +1

dend %>%
  set("labels", NULL) %>%
  plot(axes = F, main = "Hierachical distance of GO terms")

dend %>% set("branches_k_color",c("gray", "red"), k = 2) %>%  set("labels", NULL) %>%
  plot(axes = F, main = "Sample the tree: \n Get a random branch")

dend %>% set("branches_k_color",c("gray", "red"), k = 2) %>%  set("labels", NULL) %>%
  set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 1.5) %>%
  set("leaves_col", c("NA","blue")[dendCut]) %>%
  plot(axes = F, main = "Sample branch: \n Get a proportion of leaves")

#############
#next sampple

dendCut <- cutree(dendOrder, k = 5)
dendCut <- sample(names(dendCut[dendCut == 5]), size = 4, replace = F)

dendCut <- dendOrder$labels[dendOrder$order] %in% dendCut
dendCut <- as.numeric(dendCut) +1

dend %>% set("branches_k_color", c("red", "gray","gray","gray","gray"), k = 5) %>%  set("labels", NULL) %>%
  plot(axes = F, main = "Sample the tree: \n Get a random branch")

dend %>% set("branches_k_color", c("red", "gray","gray","gray","gray"), k = 5) %>%  set("labels", NULL) %>%
  set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 1) %>%
  set("leaves_col", c("NA","purple")[dendCut]) %>%
  plot(axes = F, main = "Sample branch: \n Get a proportion of leaves")

#############
#another sample


dendCut <- cutree(dendOrder, k = 5)
dendCut <- sample(names(dendCut[dendCut == 3]), size = 6, replace = F)

dendCut <- dendOrder$labels[dendOrder$order] %in% dendCut
dendCut <- as.numeric(dendCut) +1

dend %>% set("branches_k_color", c("gray", "gray","red","gray","gray"), k = 5) %>%  set("labels", NULL) %>%
  plot(axes = F, main = "Sample the tree: \n Get a random branch")

dend %>% set("branches_k_color", c("gray", "gray","red","gray","gray"), k = 5) %>%  set("labels", NULL) %>%
  set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 1) %>%
  set("leaves_col", c("NA","orange")[dendCut]) %>%
  plot(axes = F, main = "Sample branch: \n Get a proportion of leaves")

#############
#result

#4 purple
#8 blue
#6 orange

set.seed(42)
clusters <- temp <- data.frame(x = c(rep(4:5, 2),
                                     rep(5:7, 2),
                                     rep(1:3, 3)),
                               y = c(sort(rep(1:2, 2)),
                                     sort(rep(4:6, 2)),
                                     sort(rep(4:6, 3))),
                               col = c(rep("purple", 4),
                                       rep("orange", 6),
                                       rep("blue", 9)))

plot(clusters[,-3] * abs(rnorm(38, mean = 1, sd = 0.04)),
     col = clusters$col,
     pch = 16,
     cex = 3,
     axes = F,
     xlab = "", ylab = "")

dev.off()

pdf("ClusteringAlgoDevelopment/figures/schematic.walktrap.pdf", height = 5, width = 5 )

y <- data.frame(from = c(2,3,4,5,11,21,31), to = c(1,1,1,1, 12,22,32))
x <- graph_from_data_frame(y, directed = F)
V(x)$arrow.size <- 0
V(x)$label <- NA
V(x)$color <- "gray"
V(x)$size <- 12

V(x)$color[nchar(V(x)$name) == 1] <- "purple"
V(x)$color[V(x)$name %in% c(11,12)] <- "blue"
V(x)$color[V(x)$name %in% c(21,22)] <- "darkgreen"
V(x)$color[V(x)$name %in% c(31,32)] <- "darkred"

plot(x, vertex.color = "gray")
plot(x)

yy <- rbind(y, data.frame(from = c(12,22,32, 1111, 1112, 1113,1113),
                          to = c(999,999,999, 999, 1111, 1112,1)))
xx <- graph_from_data_frame(yy, directed = F)
V(xx)$arrow.size <- 0
V(xx)$label <- NA
V(xx)$color <- "gray"

V(xx)$shape <- "circle"
V(xx)$size <- 12

V(xx)$shape[nchar(V(xx)$name) >= 3]  <- "square"
# V(xx)$size[nchar(V(xx)$name) >= 3]  <- 5
V(xx)$color[nchar(V(xx)$name) >= 3] <- "black"

V(xx)$color[nchar(V(xx)$name) == 1] <- "purple"
V(xx)$color[nchar(V(xx)$name) == 2] <- "lightblue"
set.seed(42)
plot(xx)

V(xx)$color[V(xx)$name %in%  c(1:5)] <- "purple"
V(xx)$color[V(xx)$name %in% c(11,12)] <- "blue"
V(xx)$color[V(xx)$name %in% c(21,22)] <- "darkgreen"
V(xx)$color[V(xx)$name %in% c(31,32)] <- "darkred"
set.seed(42)
plot(xx)

V(xx)$color[nchar(V(xx)$name) < 4] <- "gray"
set.seed(42)
plot(xx)

plot.new()
legend(0,0.5, legend = c("Significant ontology term", "Known ontology term"), pch = 16:15, cex = 1.6, col = c("gray", "black"))
dev.off()

# pdf("ClusteringAlgoDevelopment/figures/accuracyRecallPression.pdf")
#
# set.seed(42)
# clusters <- temp <- data.frame(x = c(rep(4:5, 2),
#                                      rep(5:7, 2),
#                                      rep(1:3, 3)),
#                                y = c(sort(rep(1:2, 2)),
#                                      sort(rep(4:6, 2)),
#                                      sort(rep(4:6, 3))),
#                                col = c(rep("purple", 4),
#                                        rep("orange", 6),
#                                        rep("blue", 9)))
#
# plot(clusters[,-3] * abs(rnorm(38, mean = 1, sd = 0.04)),
#      main = "Specificity - TPR\nTrue positive/(true positive + false positive)\n ie. trust when yes",
#      sub = "TPR=1/(1+1)=0.5",
#      col = clusters$col,
#      pch = c(rep(17,10), rep(15,9)),
#      cex = 3,
#      axes = F,
#      xlab = "", ylab = "")
# legend(0.7, 2.5, c("Predicted cluster 1", "Predicted cluster 2",
#                    c("True cluster A", "True cluster B", "True clustert C")),
#        pch = c(17, 15, 16,16, 16),
#        cex = 1, bty = "n", col = c("black", "black", "purple", "orange", "blue"))
#
# plot(clusters[,-3] * abs(rnorm(38, mean = 1, sd = 0.04)),
#      main = "Specificity - TPR\nTrue positive/(true positive + false positive)\n ie. trust when yes",
#      sub = "TPR=1/(1+1)=0.5",
#      col = clusters$col,
#      pch = c(rep(17,10), rep(15,9)),
#      cex = 3,
#      axes = F,
#      xlab = "", ylab = "")
# legend(0.7, 2.5, c("Predicted cluster 1", "Predicted cluster 2",
#                    c("True cluster A", "True cluster B", "True clustert C")),
#        pch = c(17, 15, 16,16, 16),
#        cex = 1, bty = "n", col = c("black", "black", "purple", "orange", "blue"))
# dev.off()

library(dendextend)
library(magrittr)

pdf("figures/schematic.pdf", height = 5, width = 5 )

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
  set("leaves_cex", 1) %>%
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

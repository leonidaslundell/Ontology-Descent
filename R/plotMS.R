### Temporary Function for Modifying Example Data ### DELETE LATER ###
modData <- function(data, pathN, clustN){
  data <- droplevels(data[sample(1:nrow(data), pathN),])
  data$clusterNumber <- sample(1:clustN, nrow(data), replace = TRUE)
  data$clusterName <- sample(data$ontoTerm, clustN)[data$clusterNumber]

  return(data)
}


#' Plot Enrichment By Cluster
#'
#' @param ontoID Optional. A character vector of Gene Ontology IDs.
#' @param ontoTerm Optional. A character vector of Terms associated with Gene Ontology IDs.
#' @param pValue A numeric vectors of enrichment p Values for each Gene Ontology pathway.
#' @param clusterNumber Optional. A numeric vector showing which cluster the Gene Ontologies belong to.
#' @param clusterName A character vector showing terms associated with the Cluster IDs.
#' @param enrichmentScore Optional. A numeric vector of enrichment scores for each Gene Ontology.
#' @param direction Optional. A character vector containing values "Down" or "Up", indicating the direction of pathway enrichment for each Gene Ontology.
#' @param plotEnrichment Optional. TRUE or FALSE. Whether to plot the enrichment score instead of p values. Default == FALSE.
#' @param coordFlip Optional. TRUE or FALSE. With default == FALSE the plot shows cluster names on y and pValue/enrichmentScore on x axis. When TRUE x and y axes are flipped.
#' @param themeSet Optional. A character string to select the visual theme of the plot: one of "bw", "classic", "grey", "minimal" or "dark". Default == "bw"
#' @param colorSet Optional. A character string to select the color palette for the plot: one of  "Brewer", "AAAS", "D3", "Futurama", "IGV", "JAMA", "JCO", "Lancet", "LocusZoom", "NEJM", "NPG", "RickAndMorty", "Simpsons", "StarTrek", "Tron", "UChicago", or "UCSCGB". Default == "Brewer"
#' @param nameSize Optional. A numeric value setting font size for cluster names. Default == 7.
#' @param axTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param axTitleSize Optional. A numeric value setting font size for the axis title. Default = 9.
#' @param fontFam Optional. A character string to select the text font family for the plot. One of "serif", "sans" or "mono". Default == "sans".
#'
#' @return A ggplot object.
#' @import ggplot2 RColorBrewer ggsci
#' @export
#'
#' @examples
#'
clusterGraph <- function(ontoID = NULL, ontoTerm = NULL, pValue, clusterNumber = NULL, clusterName,
                         enrichmentScore = NULL, direction = NULL, plotEnrichment = FALSE, coordFlip = FALSE,
                         themeSet = "bw", colorSet = "Brewer",
                         nameSize = 8, axTxtSize = 8, axTitleSize = 10, fontFam = "sans"){

  ### Create plotting Data Frame ###
  plot <- data.frame(ontoTerm, pValue, clusterName)

  if (!is.null(ontoID)){plot$ontoID <- ontoID}
  if (!is.null(clusterNumber)){plot$clusterNumber <- clusterNumber}
  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  ### Set Color Palette ###
  col <- list("Brewer" = RColorBrewer::brewer.pal(8, "Set2"),
              "AAAS" = ggsci::pal_aaas()((10)),
              "D3" = ggsci::pal_d3()((10)),
              "Futurama" = ggsci::pal_futurama()((12)),
              "IGV" = ggsci::pal_igv()((51)),
              "JAMA" = ggsci::pal_jama()((7)),
              "JCO" = ggsci::pal_jco()((10)),
              "Lancet" = ggsci::pal_lancet()((9)),
              "LocusZoom" = ggsci::pal_locuszoom()((7)),
              "NEJM" = ggsci::pal_nejm()((8)),
              "NPG" = ggsci::pal_npg()((10)),
              "RickAndMorty" = ggsci::pal_rickandmorty()((12)),
              "Simpsons" = ggsci::pal_simpsons()((16)),
              "StarTrek" = ggsci::pal_startrek()((7)),
              "Tron" = ggsci::pal_tron()((7)),
              "UChicago" = ggsci::pal_uchicago()((9)),
              "UCSCGB" = ggsci::pal_ucscgb()((26)))

  col <- unlist(col[names(col) == colorSet])
  names(col) <- NULL

  if (length(unique(plot$clusterName)) > length(col)){col <- colorRampPalette(col)(length(unique(plot$clusterName)))}

  ### Create ggplot Object ###
  if (!isTRUE(plotEnrichment)){

    ### 1. Plot P Values (plotEnrichement == FALSE) ###
    plot <- plot[order(plot$clusterName, -log10(plot$pValue)),]
    plot$ontoID <- factor(plot$ontoID, levels = unique(plot$ontoID))
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(floor(min(-log10(plot$pValue))), ceiling(max(-log10(plot$pValue))))

    p <- ggplot2::ggplot(plot,
                         ggplot2::aes(x = -log10(pValue), y = clusterName, fill = clusterName, color = clusterName))+
      ggplot2::geom_point(pch = 21, size = 1, position = "jitter")+
      ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster", guide = "none")+
      ggplot2::scale_color_manual(values = col, name = "Cluster", guide = "none")

  } else if (isTRUE(plotEnrichment)){

    ### 2. Plot Enrichment Score (plotEnrichement == TRUE) ###
    plot <- plot[order(plot$clusterName, plot$enrichmentScore),]
    plot$ontoID <- factor(plot$ontoID, levels = unique(plot$ontoID))
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(-ceiling(max(abs(plot$enrichmentScore))),ceiling(max(abs(plot$enrichmentScore))))

    p <- ggplot2::ggplot(plot, ggplot2::aes(x = enrichmentScore, y = clusterName, fill = clusterName, color = clusterName))+
      ggplot2::geom_vline(xintercept = 0, lty = "dashed")+
      ggplot2::geom_point(pch = 21, size = 1, position = "jitter")+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster", guide = "none")+
      ggplot2::scale_color_manual(values = col, name = "Cluster", guide = "none")
  }

  ### Set Theme ###
  if (themeSet == "bw"){
    p <- p + ggplot2::theme_bw()
  } else if (themeSet == "classic"){
    p <- p + ggplot2::theme_classic()
  } else if (themeSet == "grey"){
    p <- p + ggplot2::theme_grey()
  } else if (themeSet == "minimal"){
    p <- p + ggplot2::theme_minimal()
  } else if (themeSet == "dark"){
    p <- p + ggplot2::theme_dark()
  }

  ### Other Theme Options + Coord Flip ###
  if (!isTRUE(coordFlip)){

    p <- p + ggplot2::theme(text = ggplot2::element_text(family = fontFam),
                            axis.text.y = ggplot2::element_text(size = nameSize),
                            axis.text.x = ggplot2::element_text(size = axTxtSize),
                            axis.title.y = ggplot2::element_blank(),
                            axis.title.x = ggplot2::element_text(size = axTitleSize))

  } else if (isTRUE(coordFlip)){

    p <- p + ggplot2::coord_flip()+
      ggplot2::theme(text = ggplot2::element_text(family = fontFam),
                     axis.text.x = ggplot2::element_text(size = nameSize, angle = 90, hjust = 1),
                     axis.text.y = ggplot2::element_text(size = axTxtSize),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = axTitleSize, angle = 90, hjust = .5))
  }

  return(p)
}

#' Plot Enrichment By Pathway
#'

#' @param ontoID Optional. A character vector of Gene Ontology IDs.
#' @param ontoTerm Optional. A character vector of Terms associated with Gene Ontology IDs.
#' @param pValue A numeric vectors of enrichment p Values for each Gene Ontology pathway.
#' @param clusterNumber Optional. A numeric vector showing which cluster the Gene Ontologies belong to.
#' @param clusterName A character vector showing terms associated with the Cluster IDs.
#' @param enrichmentScore Optional. A numeric vector of enrichment scores for each Gene Ontology.
#' @param direction Optional. A character vector containing values "Down" or "Up", indicating the direction of pathway enrichment for each Gene Ontology.
#' @param plotEnrichment Optional. TRUE or FALSE. Whether to plot the enrichment score instead of p values. Default == FALSE.
#' @param coordFlip Optional. TRUE or FALSE. With default == FALSE the plot shows cluster names on y and pValue/enrichmentScore on x axis. When TRUE x and y axes are flipped.
#' @param themeSet Optional. A character string to select the visual theme of the plot: one of "bw", "classic", "grey", "minimal" or "dark". Default == "bw"
#' @param colorSet Optional. A character string to select the color palette for the plot: one of  "Brewer", "AAAS", "D3", "Futurama", "IGV", "JAMA", "JCO", "Lancet", "LocusZoom", "NEJM", "NPG", "RickAndMorty", "Simpsons", "StarTrek", "Tron", "UChicago", or "UCSCGB". Default == "Brewer"
#' @param lgdPosition Optional. A character string setting the legend position: one of "right", "left", "top" or "bottom". Default == "right".
#' @param nameSize Optional. A numeric value setting font size for cluster names. Default == 7.
#' @param axTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param axTitleSize Optional. A numeric value setting font size for the axis title. Default == 9.
#' @param lgTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param lgTitleSize Optional. A numeric value setting font size for the axis text. Defualt == 9.
#' @param fontFam Optional. A character string to select the text font family for the plot. One of "serif", "sans" or "mono". Default == "sans".
#'
#' @return A ggplot object.
#' @import ggplot2 RColorBrewer ggsci
#' @export
#'
#' @examples
pathwayGraph <- function(ontoID = NULL, ontoTerm, pValue, clusterNumber = NULL, clusterName,
                         enrichmentScore = NULL, direction = NULL, plotEnrichment = FALSE, coordFlip = FALSE,
                         themeSet = "bw", colorSet = "Brewer", lgdPosition = "right",
                         nameSize = 7, axTxtSize = 7, axTitleSize = 9, lgTxtSize = 7, lgTitleSize = 9, fontFam = "sans"){

  ### Create plotting Data Frame ###
  plot <- data.frame(ontoTerm, pValue, clusterName)

  if (!is.null(ontoID)){plot$ontoID <- ontoID}
  if (!is.null(clusterNumber)){plot$clusterNumber <- clusterNumber}
  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  ### Set Color Palette ###
  col <- list("Brewer" = RColorBrewer::brewer.pal(8, "Set2"),
              "AAAS" = ggsci::pal_aaas()((10)),
              "D3" = ggsci::pal_d3()((10)),
              "Futurama" = ggsci::pal_futurama()((12)),
              "IGV" = ggsci::pal_igv()((51)),
              "JAMA" = ggsci::pal_jama()((7)),
              "JCO" = ggsci::pal_jco()((10)),
              "Lancet" = ggsci::pal_lancet()((9)),
              "LocusZoom" = ggsci::pal_locuszoom()((7)),
              "NEJM" = ggsci::pal_nejm()((8)),
              "NPG" = ggsci::pal_npg()((10)),
              "RickAndMorty" = ggsci::pal_rickandmorty()((12)),
              "Simpsons" = ggsci::pal_simpsons()((16)),
              "StarTrek" = ggsci::pal_startrek()((7)),
              "Tron" = ggsci::pal_tron()((7)),
              "UChicago" = ggsci::pal_uchicago()((9)),
              "UCSCGB" = ggsci::pal_ucscgb()((26)))

  col <- unlist(col[names(col) == colorSet])
  names(col) <- NULL

  if (length(unique(plot$clusterName)) > length(col)){col <- colorRampPalette(col)(length(unique(plot$clusterName)))}

  ### Create ggplot Object ###
  if (!isTRUE(plotEnrichment)){

    ### 1. Plot P Values (plotEnrichment == FALSE) ###
    plot <- plot[order(plot$clusterName, -log10(plot$pValue)),]
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(floor(min(-log10(plot$pValue))), ceiling(max(-log10(plot$pValue))))

    if (is.null(direction)){

      ### 1.1 Plot Without Indicating Direction (direction == NULL) ###

      p <- ggplot2::ggplot(plot, ggplot2::aes(x = -log10(pValue), y = ontoTerm, fill = clusterName, color = clusterName))+
        ggplot2::geom_point(pch = 21, size = 1)+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")

    } else if (!is.null(direction)){

      ### 1.2 Plot With Direction (direction != NULL) ###

      p <- ggplot2::ggplot(plot, ggplot2::aes(x = -log10(pValue), y = ontoTerm, fill = clusterName, color = clusterName, shape = direction))+
        ggplot2::geom_point(size = 1)+
        ggplot2::scale_shape_manual(values = c(25,24), name = "Direction")+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))
    }
  } else if (isTRUE(plotEnrichment)){

    ### 2. Plot Enrichment Score (plotEnrichment == TRUE) ###
    plot <- plot[order(plot$clusterName, plot$enrichmentScore),]
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(-ceiling(max(abs(plot$enrichmentScore))),ceiling(max(abs(plot$enrichmentScore))))

    p <- ggplot2::ggplot(plot, ggplot2::aes(x = enrichmentScore, y = ontoTerm, fill = clusterName, color = clusterName))+
      ggplot2::geom_vline(xintercept = 0, lty = "dashed")+
      ggplot2::geom_point(pch = 21, size = 1)+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster")+
      ggplot2::scale_color_manual(values = col, name = "Cluster")
  }

  ### Set Theme ###
  if (themeSet == "bw"){
    p <- p + ggplot2::theme_bw()
  } else if (themeSet == "classic"){
    p <- p + ggplot2::theme_classic()
  } else if (themeSet == "grey"){
    p <- p + ggplot2::theme_grey()
  } else if (themeSet == "minimal"){
    p <- p + ggplot2::theme_minimal()
  } else if (themeSet == "dark"){
    p <- p + ggplot2::theme_dark()
  }

  ### Other Theme Options + Coord Flip + Legend Position ###
  p <- p + ggplot2::theme(text = ggplot2::element_text(family = fontFam),
                          legend.position = lgdPosition,
                          legend.title = ggplot2::element_text(size = lgTitleSize),
                          legend.text = ggplot2::element_text(size = lgTxtSize),
                          legend.spacing.y = ggplot2::unit(0.1, "cm"))

  if (!isTRUE(coordFlip)){

    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(size = nameSize),
                            axis.text.x = ggplot2::element_text(size = axTxtSize),
                            axis.title.y = ggplot2::element_blank(),
                            axis.title.x = ggplot2::element_text(size = axTitleSize))

  } else if (isTRUE(coordFlip)){

    p <- p + ggplot2::coord_flip()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = nameSize, angle = 90, hjust = 1),
                     axis.text.y = ggplot2::element_text(size = axTxtSize),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = axTitleSize, angle = 90, hjust = .5))
  }

  return(p)

}


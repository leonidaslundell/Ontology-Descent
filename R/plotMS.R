#' Plot Enrichment by Pathway
#'
#' @param ontoID
#' @param ontoTerm
#' @param pValue
#' @param clusterNumber
#' @param clusterName
#' @param enrichmentScore optional
#' @param direction optional
#' @param plotEnrichment TRUE/FALSE defines x axis - default FALSE
#' @param coordFlip TRUE / FALSE - switch x and y axes - default FALSE
#' @param interactive TRUE/FALSE plotly or ggplot2 - default FALSE
#' @param themeSet one of "bw", "classic", "grey", "minimal" or "dark" - default "bw"
#' @param colorSet select color palette - "Set1", "NPG", "AAAS", "NEJM", "Lancet", "JCO", "UCSCGB", "D3", "IGV", "UChicago", "Futurama", "RickAndMorty", "TheSimpsons" - default "Set1"
#' @param lgdPosition one of "right", "left", "top", "bottom" - default "right"
#' @param nameSize numeric; font size for pathway / cluster names; default 10.
#' @param axTxtSize numeric; font size for axis text = numbers or Up Down; defualt 10.
#' @param axTitleSize numeric; font size for axis title = log P or Enrich Score; default 12.
#' @param lgTxtSize numer; font size for legend title; default 10.
#' @param lgdTitleSize numeric; font size for legend text; default 12.
#' @param fontFam character string; font family for text on the plot; default sans.
#'
#' @return
#' @import ggplot2 plotly RColorBrewer ggsci
#' @export
#'
#' @examples

pathwayGraph <- function(ontoID, ontoTerm, pValue, clusterNumber, clusterName, enrichmentScore = NULL, direction = NULL,
                         plotEnrichment = FALSE, coordFlip = FALSE,
                         themeSet = "bw", colorSet = "Set1", lgdPosition = "right",
                         nameSize = 10, axTxtSize = 10, axTitleSize = 12,
                         lgTxtSize = 10, lgTitleSize = 12, fontFam = "sans"){

  # Create plot data.frame depending on input
  plot <- data.frame(ontoID, ontoTerm, pValue, clusterNumber, clusterName)

  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  # Set color palette
  col <- list("Set1" = RColorBrewer::brewer.pal(9, "Set1"),
              "NPG" = ggsci::pal_npg()((10)),
              "AAAS" = ggsci::pal_aaas()((10)),
              "NEJM" = ggsci::pal_nejm()((8)),
              "Lancet" = ggsci::pal_lancet()((9)),
              "JCO" = ggsci::pal_jco()((10)),
              "UCSCGB" = ggsci::pal_ucscgb()((10)),
              "D3" = ggsci::pal_d3()((10)),
              "IGV" = ggsci::pal_igv()((10)),
              "UChicago" = ggsci::pal_uchicago()((9)),
              "Futurama" = ggsci::pal_futurama()((10)),
              "RickAndMorty" = ggsci::pal_rickandmorty()((10)),
              "TheSimpsons" = ggsci::pal_simpsons()((10)))

  col <- unlist(col[names(col) == colorSet])
  names(col) <- NULL

  if (length(unique(plot$clusterName)) > 8){col <- colorRampPalette(col)(length(unique(plot$clusterName)))}


      # Option 1: Plot P Value
  if (!isTRUE(plotEnrichment)){
    plot <- plot[order(plot$clusterNumber, -log10(plot$pValue)),]
    plot$ontoID <- factor(plot$ontoID, levels = unique(plot$ontoID))
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(floor(min(-log10(plot$pValue))), ceiling(max(-log10(plot$pValue))))

    # Option 1.1: Plot P Value - No direction
    if (is.null(direction)){
      p <- ggplot2::ggplot(plot,
                           ggplot2::aes(x = -log10(pValue), y = ontoTerm,
                            fill = clusterName, color = clusterName,
                            label = ontoID, label2 = ontoTerm, label3 = enrichmentScore))+
        ggplot2::geom_point(pch = 21, size = 3)+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")

    # Option 1.2: Plot P Value - With Direction
    } else if (!is.null(direction)){
      p <- ggplot2::ggplot(plot,
                           ggplot2::aes(x = -log10(pValue), y = ontoTerm,
                            fill = clusterName, color = clusterName, shape = direction,
                            label = ontoID, label2 = ontoTerm, label3 = enrichmentScore))+
        ggplot2::geom_point(size = 3)+
        ggplot2::scale_shape_manual(values = c(25,24), name = "Direction")+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))
    }
  }

  # Option 2: Plot Enrichment Score
  if (isTRUE(plotEnrichment)){
    plot <- plot[order(plot$clusterNumber, plot$enrichmentScore),]
    plot$ontoID <- factor(plot$ontoID, levels = unique(plot$ontoID))
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(-ceiling(max(abs(plot$enrichmentScore))),ceiling(max(abs(plot$enrichmentScore))))

    p <- ggplot2::ggplot(plot,
                         ggplot2::aes(x = enrichmentScore, y = ontoTerm,
                          fill = clusterName, color = clusterName,
                          label = ontoID, label2 = ontoTerm, label3 = pValue))+
      ggplot2::geom_vline(xintercept = 0, lty = "dashed")+
      ggplot2::geom_point(pch = 21, size = 3)+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster")+
      ggplot2::scale_color_manual(values = col, name = "Cluster")
  }


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

  if (isTRUE(coordFlip)){
    # 1. Flip Coordinates
    p <- p + ggplot2::coord_flip()+
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5, size = nameSize, family = fontFam),
                     axis.title.y = ggplot2::element_text(size = axTitleSize, family = fontFam),
                     axis.text.y = ggplot2::element_text(size = axTxtSize, family = fontFam))

    } else if (!isTRUE(coordFlip)){

      # 2. No Coordinate Flip
      p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                              axis.text.y = ggplot2::element_text(size = nameSize, family = fontFam),
                              axis.title.x = ggplot2::element_text(size = axTitleSize, family = fontFam),
                              axis.text.x = ggplot2::element_text(size = axTxtSize, family = fontFam))
    }

  ### Other Theme Options ###
  p <- p + ggplot2::theme(
    legend.position = lgdPosition,
    legend.title = ggplot2::element_text(size = lgTitleSize, family = fontFam),
    legend.text = ggplot2::element_text(size = lgTxtSize, family = fontFam))

  return(p)

}

#' Plot Enrichment by Cluster
#'
#' @param ontoID
#' @param ontoTerm
#' @param pValue
#' @param clusterNumber
#' @param clusterName
#' @param enrichmentScore optional
#' @param direction optional
#' @param plotEnrichment TRUE/FALSE defines x axis - default FALSE
#' @param coordFlip TRUE / FALSE - switch x and y axes - default FALSE
#' @param interactive TRUE/FALSE plotly or ggplot2 - default FALSE
#' @param themeSet one of "bw", "classic", "grey", "minimal" or "dark" - default "bw"
#' @param colorSet select color palette - "Set1", "NPG", "AAAS", "NEJM", "Lancet", "JCO", "UCSCGB", "D3", "IGV", "UChicago", "Futurama", "RickAndMorty", "TheSimpsons" - default "Set1"
#'
#' @return
#'
#' @import ggplot2 plotly RColorBrewer ggsci
#' @export
#'
#' @examples
clusterGraph <- function(ontoID, ontoTerm, pValue, clusterNumber, clusterName, enrichmentScore = NULL, direction = NULL,
                         plotEnrichment = FALSE, coordFlip = FALSE,
                         themeSet = "bw", colorSet = "Set1", lgdPosition = "right",
                         nameSize = 10, axTxtSize = 10, axTitleSize = 12,
                         lgTxtSize = 10, lgTitleSize = 12, fontFam = "sans"){

  # Create plot data.frame depending on input
  plot <- data.frame(ontoID, ontoTerm, pValue, clusterNumber, clusterName)

  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  # Set color palette
  col <- list("Set1" = RColorBrewer::brewer.pal(9, "Set1"),
              "NPG" = ggsci::pal_npg()((10)),
              "AAAS" = ggsci::pal_aaas()((10)),
              "NEJM" = ggsci::pal_nejm()((8)),
              "Lancet" = ggsci::pal_lancet()((9)),
              "JCO" = ggsci::pal_jco()((10)),
              "UCSCGB" = ggsci::pal_ucscgb()((10)),
              "D3" = ggsci::pal_d3()((10)),
              "IGV" = ggsci::pal_igv()((10)),
              "UChicago" = ggsci::pal_uchicago()((9)),
              "Futurama" = ggsci::pal_futurama()((10)),
              "RickAndMorty" = ggsci::pal_rickandmorty()((10)),
              "TheSimpsons" = ggsci::pal_simpsons()((10)))

  col <- unlist(col[names(col) == colorSet])
  names(col) <- NULL

  if (length(unique(plot$clusterName)) > 8){col <- colorRampPalette(col)(length(unique(plot$clusterName)))}

  # Option 1: Plot P Value
  if (!isTRUE(plotEnrichment)){
    plot <- plot[order(plot$clusterNumber, -log10(plot$pValue)),]
    plot$ontoID <- factor(plot$ontoID, levels = unique(plot$ontoID))
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(floor(min(-log10(plot$pValue))), ceiling(max(-log10(plot$pValue))))

    # Option 1.1: Plot P Value - No direction
    if (is.null(direction)){
      p <- ggplot2::ggplot(plot,
                           ggplot2::aes(x = -log10(pValue), y = clusterName,
                            fill = clusterName, color = clusterName,
                            label = ontoID, label2 = ontoTerm, label3 = enrichmentScore))+
        ggplot2::geom_point(pch = 21, size = 3, position = "jitter")+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")

      # Option 1.2: Plot P Value - With Direction
    } else if (!is.null(direction)){
      p <- ggplot2::ggplot(plot,
                           ggplot2::aes(x = -log10(pValue), y = direction,
                            fill = clusterName, color = clusterName,
                            label = ontoID, label2 = ontoTerm, label3 = enrichmentScore))+
        ggplot2::geom_point(size = 3, position = "jitter")+
        ggplot2::facet_wrap(~clusterName, ncol = 1, strip.position = "left")+
        ggplot2::scale_shape_manual(values = c(25,24), name = "Direction")+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))
    }
  }

  # Option 2: Plot Enrichment Score
  if (isTRUE(plotEnrichment)){
    plot <- plot[order(plot$clusterNumber, plot$enrichmentScore),]
    plot$ontoID <- factor(plot$ontoID, levels = unique(plot$ontoID))
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))

    xlim <- c(-ceiling(max(abs(plot$enrichmentScore))),ceiling(max(abs(plot$enrichmentScore))))

    p <- ggplot2::ggplot(plot,
                         ggplot2::aes(x = enrichmentScore, y = clusterName,
                          fill = clusterName, color = clusterName,
                          label = ontoID, label2 = ontoTerm, label3 = pValue))+
      ggplot2::geom_vline(xintercept = 0, lty = "dashed")+
      ggplot2::geom_point(pch = 21, size = 3, position = "jitter")+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster")+
      ggplot2::scale_color_manual(values = col, name = "Cluster")
  }

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


  if (isTRUE(coordFlip)){
    # 1.1 Flip Coord with Facet Wrap
    if (!isTRUE(plotEnrichment) & !is.null(direction)){
      p <- p + ggplot2::facet_wrap(~clusterName, nrow = 1, strip.position = "bottom")+
        ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(family = fontFam, size = axTxtSize),
                       axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                       axis.title.y = ggplot2::element_text(family = fontFam, size = axTitleSize),
                       strip.placement = "outside",
                       strip.text.x = ggplot2::element_text(angle = 90, hjust = 1, family = fontFam, size = nameSize),
                       strip.background = ggplot2::element_blank(),
                       panel.spacing.y = ggplot2::unit(0, "line"))
    } else if (isTRUE(plotEnrichment) | is.null(direction)){
      # 1.2 Flip Coord without Facet Wrap
      p <- p + ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, family = fontFam, size = nameSize),
                       axis.text.y = ggplot2::element_text(family = fontFam, size = axTxtSize),
                       axis.title.y = ggplot2::element_text(family = fontFam, size = axTitleSize),
                       strip.placement = "outside",
                       strip.text.y.left = ggplot2::element_text(angle = 0),
                       strip.background = ggplot2::element_blank(),
                       panel.spacing.y = ggplot2::unit(0, "line"))
      }
  } else if (!isTRUE(coordFlip)){
    # 2. No Coord Flip
    p <- p + ggplot2::theme(axis.text = ggplot2::element_text(family = fontFam, size = axTxtSize),
                            axis.title.y = ggplot2::element_blank(),
                            axis.title.x = ggplot2::element_text(family = fontFam, size = axTitleSize),
                            strip.placement = "outside",
                            strip.text.y.left = ggplot2::element_text(angle = 0, family = fontFam, size = nameSize),
                            strip.background = ggplot2::element_blank(),
                            panel.spacing.y = ggplot2::unit(0, "line"))
  }

  p <- p + ggplot2::theme(
    legend.position = lgdPosition,
    legend.title = ggplot2::element_text(size = lgTitleSize, family = fontFam),
    legend.text = ggplot2::element_text(size = lgTxtSize, family = fontFam)
  )

  return(p)

}


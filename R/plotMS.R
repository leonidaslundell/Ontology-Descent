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
#' @param interactive TRUE/FALSE plotly or ggplot2 - default FALSE
#'
#' @return
#' @import ggplot2 plotly RColorBrewer ggsci
#' @export
#'
#' @examples

pathwayGraph <- function(ontoID, ontoTerm, pValue, clusterNumber, clusterName, enrichmentScore = NULL, direction = NULL,
                         plotEnrichment = FALSE, interactive = FALSE){

  # Create plot data.frame depending on input
  plot <- data.frame(ontoID, ontoTerm, pValue, clusterNumber, clusterName)

  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  # Set color palette
  col <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))((length(unique(plot$clusterName))))

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
        ggplot2::scale_color_manual(values = col, name = "Cluster")+
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

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
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
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
      ggplot2::scale_color_manual(values = col, name = "Cluster")+
      ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  int <- plotly::ggplotly(p, tooltip = c("ontoID", "ontoTerm", "pValue", "enrichmentScore"))

  if (!isTRUE(interactive)){return(p)} else if (isTRUE(interactive)){return(int)}
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
#' @param interactive TRUE/FALSE plotly or ggplot2 - default FALSE
#'
#' @return
#'
#' @import ggplot2 plotly RColorBrewer ggsci
#' @export
#'
#' @examples
clusterGraph <- function(ontoID, ontoTerm, pValue, clusterNumber, clusterName, enrichmentScore = NULL, direction = NULL,
                         plotEnrichment = FALSE, interactive = FALSE){

  # Create plot data.frame depending on input
  plot <- data.frame(ontoID, ontoTerm, pValue, clusterNumber, clusterName)

  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  # Set color palette
  col <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))((length(unique(plot$clusterName))))

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
        ggplot2::geom_point(pch = 21, size = 3)+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")+
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

      # Option 1.2: Plot P Value - With Direction
    } else if (!is.null(direction)){
      p <- ggplot2::ggplot(plot,
                           ggplot2::aes(x = -log10(pValue), y = direction,
                            fill = clusterName, color = clusterName,
                            label = ontoID, label2 = ontoTerm, label3 = enrichmentScore))+
        ggplot2::geom_point(size = 3)+
        ggplot2::facet_wrap(~clusterName, ncol = 1, strip.position = "left")+
        ggplot2::scale_shape_manual(values = c(25,24), name = "Direction")+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
              strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 0),
              strip.background = ggplot2::element_blank(), panel.spacing.y = ggplot2::unit(0, "line"))
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
      ggplot2::geom_point(pch = 21, size = 3)+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster")+
      ggplot2::scale_color_manual(values = col, name = "Cluster")+
      ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  int <- plotly::ggplotly(p, tooltip = c("ontoID", "ontoTerm", "pValue", "enrichmentScore"))

  if (!isTRUE(interactive)){return(p)} else if (isTRUE(interactive)){return(int)}
}


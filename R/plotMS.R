#' Format Data Frame for Excel Export
#'
#' @param data descent_data$inputData
#'
#' @return
#' @export
#'
#' @examples
reorderData <- function(data){
  exp <- as.data.frame(data)
  ord <- c("ontoID", "ontoTerm", "pValue", "enrichmentScore", "direction",
           "clusterNumber", "clusterTerm", "defaultClusterNumber", "defaultClusterTerm")

  ord <- ord[ord %in% colnames(exp)]

  exp <- exp[,ord]

  return(exp)
}

#' Load Theme Icons
#'
#' @return
#' @export
#'
#' @examples
themeIcon <- function(){
  nm <- c(
    sprintf("<img src='theme_bw.png' width=75px><div class='jhr'>%s</div></img>", "bw"),
    sprintf("<img src='theme_classic.png' width=75px><div class='jhr'>%s</div></img>", "classic"),
    sprintf("<img src='theme_grey.png' width=75px><div class='jhr'>%s</div></img>", "grey"),
    sprintf("<img src='theme_minimal.png' width=75px><div class='jhr'>%s</div></img>", "minimal"),
    sprintf("<img src='theme_dark.png' width=75px><div class='jhr'>%s</div></img>", "dark"),
    sprintf("<img src='theme_linedraw.png' width=75px><div class='jhr'>%s</div></img>", "linedraw"))

  return(nm)
}

#' Shorten Ontology Terms
#'
#' @param text character vector - ontology terms
#' @param cutoff number of characters to cut the terms to
#'
#' @return character vector with shortened ontology terms
#' @export
#'
#' @examples
#'
cutText <- function(text, cutoff){
  sub <- data.frame("word" = c("\\<regulation\\>", "\\<activity\\>", "\\<positive\\>",
                               "\\<negative\\>", "involved in", "ic process", "\\<pathway\\>",
                               "\\<signaling\\>", "\\<biosynthetic\\>"),
                    "sub" = c("reg.", "act.", "pos.", "neg.", "", "ism", "path.", "sign.", "synth."))

  sub$word <- as.character(sub$word)
  sub$sub <- as.character(sub$sub)

  text[is.na(text)] <- ""

  fil.l <- nchar(text) > cutoff

  new.text <- text

  ### Fiter Out Words ###
  for(i in 1:nrow(sub)){
    new.text[fil.l] <- gsub(sub[i,1], sub[i,2], new.text[fil.l])
  }

  ### Filter Out Parentheses ###
  fil.l <- nchar(new.text) > cutoff

  new.text[fil.l] <- gsub(" *\\(.*?\\) *", "", new.text[fil.l])

  ### Cut The Rest ###
  fil.l <- nchar(new.text) > cutoff

  new.text[fil.l] <- paste(strtrim(new.text[fil.l], cutoff), "...", sep = "")

  return(new.text)
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
#' @param manualClusters Optional. TRUE or FALSE. Were the cluster terms set manually or by default algorithm. Default == FALSE.
#' @param dotSize Optional. Numeric. Dot size in plots. Default == 1.
#' @param themeSet Optional. A character string to select the visual theme of the plot: one of "bw", "classic", "grey", "minimal", "dark" or "linedraw". Default == "minimal"
#' @param colorSet Optional. A character string to select the color palette for the plot: one of  "Brewer", "AAAS", "D3", "Futurama", "IGV", "JAMA", "JCO", "Lancet", "LocusZoom", "NEJM", "NPG", "RickAndMorty", "Simpsons", "StarTrek", "Tron", "UChicago", or "UCSCGB". Default == "IGV"
#' @param nameSize Optional. A numeric value setting font size for cluster names. Default == 7.
#' @param axTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param axTitleSize Optional. A numeric value setting font size for the axis title. Default = 9.
#' @param fontFam Optional. A character string to select the text font family for the plot. One of "serif", "sans" or "mono". Default == "sans".
#' @param colorManual Optional. A character vector defining colors from the clustereR.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @import RColorBrewer
#' @import ggsci
#' @import ggbeeswarm
#' @import patchwork
#' @import ggiraph
#' @export
#'
#' @examples
#'
clusterGraph <- function(clusterName, pValue, ontoID = NULL, ontoTerm = NULL, clusterNumber = NULL,
                         enrichmentScore = NULL, direction = NULL, colorManual = NULL,
                         plotEnrichment = FALSE, manualClusters = FALSE,
                         dotSize = 1, themeSet = "minimal", colorSet = "IGV",
                         nameSize = 8, axTxtSize = 8, axTitleSize = 10, fontFam = "sans"){

  ### Create plotting Data Frame ###
  plot <- data.frame(pValue, clusterName)

  if (!is.null(ontoID)){plot$ontoID <- ontoID}
  if (!is.null(ontoTerm)){plot$ontoTerm <- ontoTerm}
  if (!is.null(clusterNumber)){plot$clusterNumber <- clusterNumber}
  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  ### Set Colors / If Not Specified Set Palette ###
  if (!is.null(colorManual)){

    col <- colorManual
    names(col) <- clusterName
    col <- col[!duplicated(names(col))]

  } else if (is.null(colorManual)){

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
  }


  ### Order Y-axis (Clusters) ###
  if (!is.null(plot$clusterNumber)){
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName[order(plot$clusterNumber)]))
  } else if (is.null(plot$clusterNumber)){
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))
  }

  ### Create ggplot Object ###
  if (!isTRUE(plotEnrichment)){

    ### 1. Plot P Values (plotEnrichement == FALSE) ###
    xlim <- c(floor(min(-log10(plot$pValue))), ceiling(max(-log10(plot$pValue))))

    p <- ggplot2::ggplot(plot,
                         ggplot2::aes(x = -log10(pValue), y = clusterName, fill = clusterName, color = clusterName))+
      ggiraph::geom_point_interactive(aes(tooltip = ontoTerm, data_id = clusterName),
                                      position = ggbeeswarm::position_quasirandom(groupOnX = FALSE),
                                      size = dotSize)+
      ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster", guide = "none")+
      ggplot2::scale_color_manual(values = col, name = "Cluster", guide = "none")

  } else if (isTRUE(plotEnrichment)){

    ### 2. Plot Enrichment Score (plotEnrichement == TRUE) ###
    xlim <- c(-ceiling(max(abs(plot$enrichmentScore))),ceiling(max(abs(plot$enrichmentScore))))

    p <- ggplot2::ggplot(plot, ggplot2::aes(x = enrichmentScore, y = clusterName, fill = clusterName, color = clusterName))+
      ggplot2::geom_vline(xintercept = 0, lty = "dashed")+
      ggiraph::geom_point_interactive(aes(tooltip = ontoTerm, data_id = clusterName),
                                      position = ggbeeswarm::position_quasirandom(groupOnX = FALSE),
                                      size = dotSize)+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster", guide = "none")+
      ggplot2::scale_color_manual(values = col, name = "Cluster", guide = "none")
  }

  ### Add Y-axis Label ###
  if(!isTRUE(manualClusters)){
    yLab <- "Prototypical Clusters"
  } else if (isTRUE(manualClusters)){
    yLab <- "User Defined Clusters"
  }

  ### Create histogram counts to place next to dotplot ###
  q <- ggplot2::ggplot(plot, aes(y = clusterName, fill = clusterName)) +
    ggiraph::geom_bar_interactive(stat = "count", ggplot2::aes(tooltip = clusterName, data_id = clusterName), show.legend = FALSE)+
    ggplot2::scale_fill_manual(values = col)+
    ggplot2::scale_x_continuous(expand = c(0,0),
                                limits = c(0, ceiling(max(table(plot$clusterName)) * 1.1)),
                                breaks = ceiling(seq(0, max(table(plot$clusterName)) * 1.1, length.out = 3)))+
    ggplot2::scale_y_discrete(yLab, position = "right")+
    ggplot2::xlab("Counts")

  ### Set Theme ###
  if (themeSet == "bw"){
    p <- p + ggplot2::theme_bw()
    q <- q + ggplot2::theme_bw()

  } else if (themeSet == "classic"){
    p <- p + ggplot2::theme_classic()
    q <- q + ggplot2::theme_classic()

  } else if (themeSet == "grey"){
    p <- p + ggplot2::theme_grey()
    q <- q + ggplot2::theme_gray()

  } else if (themeSet == "minimal"){
    p <- p + ggplot2::theme_minimal()
    q <- q + ggplot2::theme_minimal()

  } else if (themeSet == "dark"){
    p <- p + ggplot2::theme_dark()
    q <- q + ggplot2::theme_dark()

  } else if (themeSet == "linedraw"){
    p <- p + ggplot2::theme_linedraw()
    q <- q + ggplot2::theme_linedraw()

  }

  ### Other Theme Options ###
  q <- q + theme(text = ggplot2::element_text(family = fontFam),
                 axis.text.x = ggplot2::element_text(size = axTxtSize),
                 axis.title.x = ggplot2::element_text(size = axTitleSize),
                 axis.title.y.right = ggplot2::element_text(size = axTitleSize),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())

  p <- p + ggplot2::theme(text = ggplot2::element_text(family = fontFam),
                          axis.text.y = ggplot2::element_text(size = nameSize),
                          axis.text.x = ggplot2::element_text(size = axTxtSize),
                          axis.title.y = ggplot2::element_blank(),
                          axis.title.x = ggplot2::element_text(size = axTitleSize))+
    ggplot2::coord_cartesian(clip = "off")

  p <- p + q + patchwork::plot_layout(widths = c(.7, .3))

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
#' @param manualClusters Optional. TRUE or FALSE. Were the cluster terms set manually or by default algorithm. Default == FALSE.
#' @param dotSize Optional. Numeric. Dot size in plots. Default == 2.
#' @param themeSet Optional. A character string to select the visual theme of the plot: one of "bw", "classic", "grey", "minimal", "dark" or "linedraw". Default == "minimal"
#' @param colorSet Optional. A character string to select the color palette for the plot: one of  "Brewer", "AAAS", "D3", "Futurama", "IGV", "JAMA", "JCO", "Lancet", "LocusZoom", "NEJM", "NPG", "RickAndMorty", "Simpsons", "StarTrek", "Tron", "UChicago", or "UCSCGB". Default == "IGV"
#' @param lgdPosition Optional. A character string setting the legend position: one of "right", "left", "top" or "bottom". Default == "bottom".
#' @param nameSize Optional. A numeric value setting font size for cluster names. Default == 7.
#' @param axTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param axTitleSize Optional. A numeric value setting font size for the axis title. Default == 9.
#' @param lgTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param lgTitleSize Optional. A numeric value setting font size for the axis text. Defualt == 9.
#' @param fontFam Optional. A character string to select the text font family for the plot. One of "serif", "sans" or "mono". Default == "sans".
#' @param colorManual Optional. A character vector defining colors from the clustereR.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @import RColorBrewer
#' @import ggsci
#' @import patchwork
#' @import ggiraph
#' @export
#'
#' @examples
pathwayGraph <- function(ontoTerm, pValue, clusterName, ontoID = NULL, clusterNumber = NULL,
                         enrichmentScore = NULL, direction = NULL, colorManual = NULL,
                         plotEnrichment = FALSE, manualClusters = FALSE,
                         dotSize = 2, themeSet = "minimal", colorSet = "IGV", lgdPosition = "bottom",
                         nameSize = 7, axTxtSize = 7, axTitleSize = 9, lgTxtSize = 7, lgTitleSize = 9, fontFam = "sans"){

  ### Create plotting Data Frame ###
  plot <- data.frame(ontoTerm, pValue, clusterName)

  if (!is.null(ontoID)){plot$ontoID <- ontoID}
  if (!is.null(clusterNumber)){plot$clusterNumber <- clusterNumber}
  if (!is.null(enrichmentScore)){plot$enrichmentScore <- enrichmentScore}
  if (!is.null(direction)){plot$direction <- direction}

  ### Set Colors / If Not Specified Set Palette ###
  if (!is.null(colorManual)){

    col <- colorManual
    names(col) <- clusterName
    col <- col[!duplicated(names(col))]

  } else if (is.null(colorManual)){

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
  }

  ### Order Y-axis (Clusters) ###
  if (!is.null(plot$clusterNumber)){
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName[order(plot$clusterNumber)]))
  } else if (is.null(plot$clusterNumber)){
    plot$clusterName <- factor(plot$clusterName, levels = unique(plot$clusterName))
  }

  ### Add Legend Label ###
  if(!isTRUE(manualClusters)){
    legLab <- "Prototypical Clusters"
  } else if (isTRUE(manualClusters)){
    legLab <- "User Defined Clusters"
  }

  ### Create ggplot Object ###
  if (!isTRUE(plotEnrichment)){

    ### 1. Plot P Values (plotEnrichment == FALSE) ###
    plot <- plot[order(plot$clusterName, -log10(plot$pValue)),]
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))

    xlim <- c(floor(min(-log10(plot$pValue))), ceiling(max(-log10(plot$pValue))))

    if (is.null(direction)){

      ### 1.1 Plot Without Indicating Direction (direction == NULL) ###

      p <- ggplot2::ggplot(plot, ggplot2::aes(x = -log10(pValue), y = ontoTerm, fill = clusterName, color = clusterName))+
        ggiraph::geom_point_interactive(size = dotSize, ggplot2::aes(tooltip = clusterName, data_id = clusterName))+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::scale_fill_manual(values = col, name = legLab)+
        ggplot2::scale_color_manual(values = col, name = legLab)

    } else if (!is.null(direction)){

      ### 1.2 Plot With Direction (direction != NULL) ###

      p <- ggplot2::ggplot(plot, ggplot2::aes(x = -log10(pValue), y = ontoTerm, fill = clusterName, color = clusterName, shape = direction))+
        ggiraph::geom_point_interactive(size = dotSize, ggplot2::aes(tooltip = clusterName, data_id = clusterName))+
        ggplot2::scale_shape_manual(values = c(25,24), name = "Direction")+
        ggplot2::scale_fill_manual(values = col, name = legLab)+
        ggplot2::scale_color_manual(values = col, name = legLab)+
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
      ggplot2::geom_point(size = dotSize)+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = legLab)+
      ggplot2::scale_color_manual(values = col, name = legLab)
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
  } else if (themeSet == "linedraw"){
    p <- p + ggplot2::theme_linedraw()
  }

  ### Other Theme Options ###
  if(is.null(lgTxtSize)){
    hw <- 7
    th <- 9
  } else if (!is.null(lgTxtSize)){
    hw <- lgTxtSize
    th <- lgTitleSize
  }

  p <- p + ggplot2::theme(text = ggplot2::element_text(family = fontFam),
                          legend.position = lgdPosition, legend.justification = "left",
                          legend.margin = ggplot2::margin(1, 1, 1, 0, unit = "pt"),
                          legend.spacing.x = ggplot2::unit(2, "pt"),
                          legend.spacing.y = ggplot2::unit(1, "pt"),
                          legend.key.height = ggplot2::unit(hw, "pt"),
                          legend.key.width = ggplot2::unit(hw, "pt"),
                          legend.title = ggplot2::element_text(size = th),
                          legend.text = ggplot2::element_text(size = hw),
                          axis.text.y = ggplot2::element_text(size = nameSize),
                          axis.text.x = ggplot2::element_text(size = axTxtSize),
                          axis.title.y = ggplot2::element_blank(),
                          axis.title.x = ggplot2::element_text(size = axTitleSize))+
    ggplot2::guides("shape" = ggplot2::guide_legend(ncol = 1, order = 1, title.position = "top"),
                    "color" = ggplot2::guide_legend(ncol = 1, title.position = "top", order = 0, reverse = TRUE),
                    "fill" = ggplot2::guide_legend(ncol = 1, title.position = "top", order = 0, reverse = TRUE))+
    ggplot2::coord_cartesian(clip = "off")

  return(p)
}


#' Create Error Messages
#'
#' @param type character string "empty" or "long", depending on the error message
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
errorMessage <- function(type){
  if (type == "empty"){
    p <- ggplot2::ggplot(data = NULL,
                         ggplot2::aes(1, 1, label = "Please enter data and cluster before plotting results."))+
      ggplot2::geom_text(size = 5)+
      ggplot2::theme_void()
  } else if (type == "long"){
    p <- ggplot2::ggplot(data = NULL,
                         ggplot2::aes(1, 1, label = "Please use Plot by Cluster to avoid overplotting.\nThe pathway graph is restricted to data sets with\n50 Pathways and 10 Clusters or less."))+
      ggplot2::geom_text(size = 5)+
      ggplot2::theme_void()
  }

  return(p)
}

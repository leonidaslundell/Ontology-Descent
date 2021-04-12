
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

#' Load Ggplot Shape Icons
#'
#' @return
#' @export
#'
#' @examples
pchIcon <- function(){
  nm <- c()

  for(i in 1:25){
    nm <- c(nm,
            sprintf(
              paste("<img src='s", i, ".png' ", "width=50px><div class='jhr'>%s</div></img>", sep = ""),
              paste("shape", i, sep = " ")
            )
    )
  }

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
#' @param coordFlip Optional. TRUE or FALSE. With default == FALSE the plot shows cluster names on y and pValue/enrichmentScore on x axis. When TRUE x and y axes are flipped.
#' @param dotSize Optional. Numeric. Dot size in plots. Default == 1.
#' @param dotShape Optional. Numeric. Select ggplot2 pch shape. Default == 19.
#' @param themeSet Optional. A character string to select the visual theme of the plot: one of "bw", "classic", "grey", "minimal", "dark" or "linedraw". Default == "minimal"
#' @param colorSet Optional. A character string to select the color palette for the plot: one of  "Brewer", "AAAS", "D3", "Futurama", "IGV", "JAMA", "JCO", "Lancet", "LocusZoom", "NEJM", "NPG", "RickAndMorty", "Simpsons", "StarTrek", "Tron", "UChicago", or "UCSCGB". Default == "IGV"
#' @param nameSize Optional. A numeric value setting font size for cluster names. Default == 7.
#' @param axTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param axTitleSize Optional. A numeric value setting font size for the axis title. Default = 9.
#' @param fontFam Optional. A character string to select the text font family for the plot. One of "serif", "sans" or "mono". Default == "sans".
#'
#' @return A ggplot object.
#' @import ggplot2
#' @import RColorBrewer
#' @import ggsci
#' @import ggbeeswarm
#' @export
#'
#' @examples
#'
clusterGraph <- function(clusterName, pValue, ontoID = NULL, ontoTerm = NULL, clusterNumber = NULL,
                         enrichmentScore = NULL, direction = NULL, colorManual = NULL,
                         plotEnrichment = FALSE, coordFlip = FALSE,
                         dotSize = 1, dotShape = 19,
                         themeSet = "minimal", colorSet = "IGV",
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
      ggbeeswarm::geom_quasirandom(groupOnX = FALSE, pch = dotShape, size = dotSize)+
      ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster", guide = "none")+
      ggplot2::scale_color_manual(values = col, name = "Cluster", guide = "none")

  } else if (isTRUE(plotEnrichment)){

    ### 2. Plot Enrichment Score (plotEnrichement == TRUE) ###
    xlim <- c(-ceiling(max(abs(plot$enrichmentScore))),ceiling(max(abs(plot$enrichmentScore))))

    p <- ggplot2::ggplot(plot, ggplot2::aes(x = enrichmentScore, y = clusterName, fill = clusterName, color = clusterName))+
      ggplot2::geom_vline(xintercept = 0, lty = "dashed")+
      ggbeeswarm::geom_quasirandom(groupOnX = FALSE, pch = dotShape, size = dotSize)+
      ggplot2::scale_x_continuous("Enrichment Score", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
      ggplot2::scale_fill_manual(values = col, name = "Cluster", guide = "none")+
      ggplot2::scale_color_manual(values = col, name = "Cluster", guide = "none")
  }

  ### Create histogram counts to place next to dotplot ###
  q <- ggplot(plot, aes(y=clusterName, fill = clusterName)) +
    stat_count(show.legend = F) +
    scale_fill_manual(values = col) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0, ceiling(max(table(plot$clusterName)) * 1.1)),
                       breaks = ceiling(seq(0, max(table(plot$clusterName)) * 1.1, length.out = 3))) +
    scale_y_discrete(position = "right") +
    xlab("Counts") +
    ylab("Pathways")

  themeQ <- theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())

  ### Set Theme ###
  if (themeSet == "bw"){
    p <- p + ggplot2::theme_bw() + theme(axis.text.x = element_text(size = 8))
    q <- q + ggplot2::theme_bw()  + theme(axis.text.x = element_text(size = 8),
                                          axis.text.y = element_blank())#+ themeQ + theme(panel.border = element_rect(size = 1))

  } else if (themeSet == "classic"){
    p <- p + ggplot2::theme_classic()
    q <- q + ggplot2::theme_classic() + themeQ

  } else if (themeSet == "grey"){
    p <- p + ggplot2::theme_grey()
    q <- q + ggplot2::theme_gray() + themeQ

  } else if (themeSet == "minimal"){
    p <- p + ggplot2::theme_minimal()
    q <- q + ggplot2::theme_minimal() + themeQ

  } else if (themeSet == "dark"){
    p <- p + ggplot2::theme_dark()
    q <- q + ggplot2::theme_dark() + themeQ

  } else if (themeSet == "linedraw"){
    p <- p + ggplot2::theme_linedraw()
    q <- q + ggplot2::theme_linedraw() + themeQ

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

  p <- p + ggplot2::coord_cartesian(clip = "off")

  p <- p + q +
    plot_layout(widths = c(.7, .3))

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
#' @param dotSize Optional. Numeric. Dot size in plots. Default == 2.
#' @param dotShape Optional. Numeric. Select ggplot2 pch shape. Default == 19.
#' @param themeSet Optional. A character string to select the visual theme of the plot: one of "bw", "classic", "grey", "minimal", "dark" or "linedraw". Default == "minimal"
#' @param colorSet Optional. A character string to select the color palette for the plot: one of  "Brewer", "AAAS", "D3", "Futurama", "IGV", "JAMA", "JCO", "Lancet", "LocusZoom", "NEJM", "NPG", "RickAndMorty", "Simpsons", "StarTrek", "Tron", "UChicago", or "UCSCGB". Default == "IGV"
#' @param lgdPosition Optional. A character string setting the legend position: one of "right", "left", "top" or "bottom". Default == "right".
#' @param nameSize Optional. A numeric value setting font size for cluster names. Default == 7.
#' @param axTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param axTitleSize Optional. A numeric value setting font size for the axis title. Default == 9.
#' @param lgTxtSize Optional. A numeric value setting font size for the axis text. Defualt == 7.
#' @param lgTitleSize Optional. A numeric value setting font size for the axis text. Defualt == 9.
#' @param fontFam Optional. A character string to select the text font family for the plot. One of "serif", "sans" or "mono". Default == "sans".
#'
#' @return A ggplot object.
#' @import ggplot2
#' @import RColorBrewer
#' @import ggsci
#' @export
#'
#' @examples
pathwayGraph <- function(ontoTerm, pValue, clusterName, ontoID = NULL, clusterNumber = NULL,
                         enrichmentScore = NULL, direction = NULL, colorManual = NULL,
                         plotEnrichment = FALSE, coordFlip = FALSE,
                         dotSize = 2, dotShape = 19,
                         themeSet = "minimal", colorSet = "IGV", lgdPosition = "right",
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

  ### Create ggplot Object ###
  if (!isTRUE(plotEnrichment)){

    ### 1. Plot P Values (plotEnrichment == FALSE) ###
    plot <- plot[order(plot$clusterName, -log10(plot$pValue)),]
    plot$ontoTerm <- factor(plot$ontoTerm, levels = unique(plot$ontoTerm))

    xlim <- c(floor(min(-log10(plot$pValue))), ceiling(max(-log10(plot$pValue))))

    if (is.null(direction)){

      ### 1.1 Plot Without Indicating Direction (direction == NULL) ###

      p <- ggplot2::ggplot(plot, ggplot2::aes(x = -log10(pValue), y = ontoTerm, fill = clusterName, color = clusterName))+
        ggplot2::geom_point(pch = dotShape, size = dotSize)+
        ggplot2::scale_x_continuous("-log10 P Value", limits = xlim, breaks = seq(xlim[1], xlim[2], 1))+
        ggplot2::scale_fill_manual(values = col, name = "Cluster")+
        ggplot2::scale_color_manual(values = col, name = "Cluster")

    } else if (!is.null(direction)){

      ### 1.2 Plot With Direction (direction != NULL) ###

      p <- ggplot2::ggplot(plot, ggplot2::aes(x = -log10(pValue), y = ontoTerm, fill = clusterName, color = clusterName, shape = direction))+
        ggplot2::geom_point(size = dotSize)+
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
      ggplot2::geom_point(pch = dotShape, size = dotSize)+
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
  } else if (themeSet == "linedraw"){
    p <- p + ggplot2::theme_linedraw()
  }

  ### Other Theme Options + Coord Flip + Legend Position ###
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
                          legend.text = ggplot2::element_text(size = hw))+
    ggplot2::guides("shape" = ggplot2::guide_legend(ncol = 1, order = 1, title.position = "top"),
                    "color" = ggplot2::guide_legend(ncol = 1, title.position = "top", order = 0),
                    "fill" = ggplot2::guide_legend(ncol = 1, title.position = "top", order = 0))

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

  p <- p + ggplot2::coord_cartesian(clip = "off")

  return(p)
}



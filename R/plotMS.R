#' Title
#'
#' @param xval
#' @param yval
#' @param fill
#'
#' @return
#'
#' @import ggplot2
#' @export
#'
#' @examples
pathwayGraph <- function(xval, yval, fill){
    ggplot2::ggplot(data = NULL, aes(xval, yval, fill = fill))+
    geom_point(pch = 21, size = 5)+
    theme_bw()+xlab("-log10 P Values")+
    theme(axis.title.y = element_blank(), legend.title = element_blank(),
          legend.justification = "top")
}

#' Title
#'
#' @param xval
#' @param yval
#'
#' @return
#'
#' @import ggplot2
#' @export
#'
#' @examples
clusterGraph <- function(xval, yval){
  ggplot2::ggplot(data = NULL, aes(xval, yval, fill = yval))+
    geom_point(pch = 21, size = 5)+
    theme_bw()+xlab("-log10 P Values")+
    theme(axis.title.y = element_blank(), legend.position = "none")
}

#' UI for the clustering page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
exploring_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  fluidPage(

    titlePanel("Cluster and explore"),

    sidebarLayout(
      sidebarPanel(
        actionButton(ns("clusterButton"),
                     label = "Cluster!")
        ),

      mainPanel(
        plotOutput(outputId = ns("netPlotOut"), height = 750,
                   brush = ns("netSelect"))
      )
    )
  )
}

#' Server for the clustering page
#'
#' @param input shiny parameter
#' @param output shiny parameter
#' @param session shiny parameter
#' @param descent_data reactiveValues, contains gene ontology data
#'
#' @import shiny
#' @export
exploring_page <- function(input, output, session, descent_data)
{
  observeEvent(input$clusterButton,{

    results <- clustereR(ontoNet = descent_data$net,
                         method = "leiden",
                         target = descent_data$inputData$ontoID)


    descent_data$inputData <- merge(descent_data$inputData[,colnames(descent_data$inputData) %in%
                                                             c("ontoID",
                                                               "direction",
                                                               "pValue",
                                                               "enrichmentScore"), with = F],
                                    results$res,
                                    by = "ontoID", order = F)

    descent_data$clustered <- list(exists =  T)

    output$netPlotOut <- renderPlot({
      par(mar = c(0,0,0,0))
      plot(results$plot,
           vertex.label = NA,
           vertex.label.cex = 0.5,
           vertex.border.cex = 0.000001,
           asp = 0,
           axes = F)
    })

  })
}

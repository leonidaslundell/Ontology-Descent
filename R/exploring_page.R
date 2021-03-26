#' UI for the clustering page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
exploring_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  fluidPage( # flexible layout function

    # Title
    titlePanel("Run clustering and explore data"),

    sidebarLayout(  # standard inputs on sidebar, outputs in main area layout
      sidebarPanel( # sidebar configuration
        actionButton(ns("clusterButton"),      # this is the name of the
                     # variable- this will be passed to server.R
                     label = "Cluster!")
        ),

      # Show a plot of the generated distribution
      mainPanel( # main output configuration
        plotOutput(outputId = ns("netPlotOut"), height = 750)
        # element as defined in server.R
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
    results <- clustereR(ontoNet = net,
                         ontoNames = GOnames,
                         ontoLength = GOlength,
                         target = descent_data$inputData$ontoID)

    # descent_data$inputData
    descent_data$inputData <- merge(descent_data$inputData[,c("ontoID", "direction", "pValue", "enrichmentScore")],
                                    results$res, by = "ontoID", order = F)

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

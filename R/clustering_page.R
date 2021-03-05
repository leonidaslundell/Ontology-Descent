#' UI for the clustering page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
clustering_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  fluidPage( # flexible layout function

    # Title
    titlePanel("Clustering"),

    sidebarLayout(  # standard inputs on sidebar, outputs in main area layout
      sidebarPanel( # sidebar configuration
        actionButton( ns("clusterButton"),      # this is the name of the
                      # variable- this will be passed to server.R
                      label = "Cluster!")

        ),

      # Show a plot of the generated distribution
      mainPanel( # main output configuration
        h3("This is you saying it"), # title with HTML helper
        textOutput(ns("textDisplay"))    # this is the name of the output
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
clustering_page <- function(input, output, session, descent_data)
{
  load(here::here("R/dataClustereR.rda"))

  observeEvent(input$clusterButton,{
    x <- clustereR(ontoNet = net,
                   ontoNames = GOnames,
                   ontoLength = GOlength,
                   target = descent_data$inputData$ontoID)

    x <- x[match(descent_data$inputData$ontoID, x$ontoID),]

    descent_data$inputData$clusterNumber <- x$cluster
    descent_data$inputData$clusterName <- x$clusterTerm

  })

  # load("data/dataClustereR.Rdata")
  # descent_data$inputData
  #write some kind of function that identifies whether the GO terms are MF/BP/CC


}

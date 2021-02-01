#' UI for the plotting page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @import ggplot2 plotly RColorBrewer
#' @export
plotting_page_ui <- function(id)
{
  ns <- shiny::NS(id)

  fluidPage(
    titlePanel("Ontology Descent: Plotting Enrichment"),

    fluidRow(
      column(width = 4,
             wellPanel(
               radioButtons(inputId = ns("plotType"), label = "Plot Type",
                            choices = c("By Pathway" = "pth", "By Cluster" = "clust"),
                            selected = "clust"),

               radioButtons(inputId = ns("axisType"), label = "Axis Type",
                            choices = c("P Value" = "p", "Enrichment Score" = "e"),
                            selected = "p"),

               actionButton(inputId = ns("actPlot"), label = "Show plot")
             )
      ),

      column(width = 8,
             plotOutput(outputId = ns("plotOut")),
             textOutput(outputId = ns("pathWarning"))
             )
      )
  )
}

#' Server for the plotting page
#'
#' @param input shiny parameter
#' @param output shiny parameter
#' @param session shiny parameter
#' @param descent_data reactiveValues, contains gene ontology data
#'
#' @import shiny
#' @export
plotting_page <- function(input, output, session, descent_data)
{
  # reactive value (ggplot)
  # render plot (based on reactive value)
  observeEvent(input$actPlot, {
    aT <- switch (input$axisType,
      "p" = FALSE,
      "e" = TRUE
      )

    if (input$plotType == "pth"){
      dat <- reactive({example_data[1:50,-c(3,5)]})

      output$plotOut <- renderPlot({pathwayGraph(ontoID = dat()$ontoID,
                                                 ontoTerm = dat()$ontoTerm,
                                                 pValue = dat()$pValue,
                                                 clusterNumber = dat()$clusterNumber,
                                                 clusterName = dat()$clusterName,
                                                 enrichmentScore = dat()$enrichmentScore,
                                                 direction = dat()$direction,
                                                 plotEnrichment = aT,
                                                 interactive = FALSE)})

      len <- reactive({nrow(example_data)})

      if (len() > 50){
        output$pathWarning <- renderText("Warning: Plot by pathway supports up to 50 separate ontoIDs.
                                         The output has been cut to top 50 ontoIDs.
                                         To plot >50 ontoIDs silmultaneously, please use plot by cluster.")
      }

    } else if (input$plotType == "clust"){
      dat <- reactive({example_data[,-c(3,5)]})

      output$plotOut <- renderPlot({clusterGraph(ontoID = dat()$ontoID,
                                                 ontoTerm = dat()$ontoTerm,
                                                 pValue = dat()$pValue,
                                                 clusterNumber = dat()$clusterNumber,
                                                 clusterName = dat()$clusterName,
                                                 enrichmentScore = dat()$enrichmentScore,
                                                 direction = dat()$direction,
                                                 plotEnrichment = aT,
                                                 interactive = FALSE)})

      output$pathWarning <- renderText("")
    }
  })
}

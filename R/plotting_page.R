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

             h4("Plot Options:"),

             inputPanel(
               radioButtons(inputId = ns("plotType"), label = "Plot Type",
                            choices = c("By Pathway" = "pth", "By Cluster" = "clust"),
                            selected = "clust"),

               radioButtons(inputId = ns("axisType"), label = "Axis Type",
                            choices = c("P Value" = "p", "Enrichment Score" = "e"),
                            selected = "p"),

               actionButton(inputId = ns("actPlot"), label = "Show plot")
             ),

             h4("Download Options:"),

             inputPanel(
               numericInput(inputId = ns("plotHt"), label = "Height", min = 2, max = 50,
                            value = 14.5, step = .5),

               numericInput(inputId = ns("plotWd"), label = "Width", min = 2, max = 50,
                            value = 9.5, step = .5),

               selectInput(inputId = ns("plotUnit"), label = "Units",
                           choices = c("cm", "in", "mm"),
                           selected = "cm", multiple = FALSE),

               numericInput(inputId = ns("plotDPI"), label = "DPI", min = 75, max = 1000,
                            value = 300, step = 25),

               selectInput(inputId = ns("fileType"), label = "File Type",
                           choices = c("tiff", "png", "eps", "ps", "tex", "pdf",
                                       "jpeg", "bmp", "svg", "wmf"),
                           selected = "tiff", multiple = FALSE),
               br(),

               downloadButton(outputId = ns("plotDwnld"), label = "Download Plot")
               )
             ),

      column(width = 8,
             plotOutput(outputId = ns("plotOut"), height = "500px"),
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
  reacVals <- reactiveValues()

  ### Plot Options ###
  observeEvent(input$actPlot, {

    axType <- switch (input$axisType, "p" = FALSE, "e" = TRUE)

    if (input$plotType == "pth"){
      dat <- reactive({example_data[order(example_data$pValue)[1:50],]})

      plotOut <- reactive({pathwayGraph(ontoID = dat()$ontoID,
                                        ontoTerm = dat()$ontoTerm,
                                        pValue = dat()$pValue,
                                        clusterNumber = dat()$clusterNumber,
                                        clusterName = dat()$clusterName,
                                        enrichmentScore = dat()$enrichmentScore,
                                        direction = dat()$direction,
                                        plotEnrichment = axType,
                                        interactive = FALSE)})

      len <- reactive({nrow(example_data)})

      if (len() > 50){
        output$pathWarning <- renderText("Warning: Plot by pathway supports up to 50 separate ontoIDs.
                                         The output has been cut to top 50 ontoIDs.
                                         To plot >50 ontoIDs silmultaneously, please use plot by cluster.")
      }

    } else if (input$plotType == "clust"){
      dat <- reactive({example_data})

      plotOut <- reactive({clusterGraph(ontoID = dat()$ontoID,
                                        ontoTerm = dat()$ontoTerm,
                                        pValue = dat()$pValue,
                                        clusterNumber = dat()$clusterNumber,
                                        clusterName = dat()$clusterName,
                                        enrichmentScore = dat()$enrichmentScore,
                                        direction = dat()$direction,
                                        plotEnrichment = axType,
                                        interactive = FALSE)})

      output$pathWarning <- renderText("")
    }

    observeEvent(input$actPlot,
                 output$plotOut <- renderPlot({
                   reacVals$plot <- plotOut()
                   print(plotOut())
                 })
    )
})

  ### Download Options ###
  reacVals$plotUnit <- reactive(switch(input$plotUnit, "cm" = "cm", "in" = "in", "mm" = "mm"))

  reacVals$plotHt <- reactive(input$plotHt)

  reacVals$plotWd <- reactive(input$plotWd)

  reacVals$plotDPI <- reactive(input$plotDPI)

  reacVals$fileType <- reactive(switch(input$fileType, "tiff" ="tiff", "png" = "png", "eps" = "eps", "ps" = "ps",
                                       "tex" = "tex", "pdf" = "pdf", "jpeg" = "jpeg", "bmp" = "bmp", "svg" = "svg",
                                       "wmf" = "wmf"))

  output$plotDwnld <- downloadHandler(
    filename = function() {paste("plot", reacVals$fileType(), sep=".")},
    content = function(file) {
      ggplot2::ggsave(file, plot = reacVals$plot, device = reacVals$fileType(),
                      width = reacVals$plotWd(), height = reacVals$plotHt(),
                      units = reacVals$plotUnit(), dpi = reacVals$plotDPI())
    }
  )
}

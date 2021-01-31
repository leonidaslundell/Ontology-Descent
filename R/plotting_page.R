#' UI for the plotting page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
plotting_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  fluidPage( # flexible layout function

    # Title
    titlePanel("Descent Plot"),

    sidebarLayout(  # standard inputs on sidebar, outputs in main area layout
      sidebarPanel( # sidebar configuration
        selectInput(inputId = ns("plotType"),
                    label = "Plot Type:",
                    choices = c("By Cluster" = "clust", "By Pathway" = "path"),
                    multiple = FALSE),

        actionButton(inputId = ns("actPlot"), "Plot"),
        numericInput(inputId = ns("ht"), label = "Height (cm)", value = 5.75),
        numericInput(inputId = ns("wd"), label = "Width (cm)", value = 8.5),
        selectInput(inputId = ns("fileType"), label = "File Type:",
                    choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"),
                    multiple = FALSE, selected = "tiff"),
        downloadButton(ns("downPlot"), "Download")
      ),


      # Show a plot of the generated distribution
      mainPanel( # main output configuration
        plotOutput(ns("plot"))
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
    if (input$plotType == "path"){
      output$plot <- renderPlot(pathwayGraph(-log10(descent_data$clusterData$pValue),
                                                                         descent_data$clusterData$ontoTerm,
                                                                         descent_data$clusterData$clusterName))
    } else if (input$plotType == "clust"){
      output$plot <- renderPlot(clusterGraph(-log10(descent_data$clusterData$pValue),
                                             descent_data$clusterData$clusterName))
    }
    })

  output$downloadData <- downloadHandler(
    filename = function(){paste("plot", input$fileType, sep = ".")},
    content = function(file){
      ggplot2::ggsave(output$plot, filename)
    }
  )

}

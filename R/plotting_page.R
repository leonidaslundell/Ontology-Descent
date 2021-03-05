#' UI for the plotting page
#'
#' @param id unique id for this module
#'
#' @import shiny ggiraph
#' @export
plotting_page_ui <- function(id)
{
  ns <- shiny::NS(id)

  fluidPage(
    titlePanel("Ontology Descent: Plotting Enrichment"),

    sidebarLayout(
      sidebarPanel(
        ### For Modifying the Test Data ###
        ### DELETE IN FINAL VERSION ###
        numericInput(inputId = ns("clustN"), label = "Cluster Number", value = 10, min = 5, max = 1000, step = 5),
        numericInput(inputId = ns("pathN"), label = "Pathway Number", value = 50, min = 5, max = 10000, step = 5),
        ####################################

        selectInput(inputId = ns("plotType"), label = "Plot Type:", choices = NULL, selected = NULL, multiple = FALSE),

        checkboxInput(inputId = ns("axisType"), label = "Plot enrichmentScore (replaces pValue)", value = FALSE),

        checkboxInput(inputId = ns("coordFlip"), label = "Flip X and Y axes", value = FALSE),

        actionButton(inputId = ns("actPlot"), label = "Show plot"),

        tabsetPanel(id = "Graphical Options",

                    tabPanel(title = "Style:",
                             selectInput(inputId = ns("themeSet"), label = "Plot Theme",
                                         choices = c("bw", "classic", "grey", "minimal", "dark"),
                                         selected = "minimal", multiple = FALSE),

                             selectInput(inputId = ns("colorSet"), label = "Color Palette",
                                         choices = c("Brewer", "AAAS", "D3", "Futurama", "IGV",
                                                     "JAMA", "JCO", "Lancet", "LocusZoom", "NEJM",
                                                     "NPG", "RickAndMorty", "Simpsons", "StarTrek",
                                                     "Tron", "UChicago", "UCSCGB"),
                                         selected = "IGV", multiple = FALSE),

                             selectInput(inputId = ns("lgdPosition"), label = "Legend Position",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                             ),

                    tabPanel(title = "Text:",
                             numericInput(ns("nameSize"), label = "Pathway (size)",
                                          value = 7, min = 4, max = 96, step = 1),

                             numericInput(ns("axTxtSize"), label = "Axis text (size)",
                                          value = 7, min = 4, max = 96, step = 1),

                             numericInput(ns("axTitleSize"), label = "Axis title (size)",
                                          value = 9, min = 4, max = 96, step = 1),

                             numericInput(ns("lgTxtSize"), label = "Legend text (size)",
                                          value = 7, min = 4, max = 96, step = 1),

                             numericInput(ns("lgTitleSize"), label = "Legend title (size)",
                                          value = 9, min = 4, max = 96, step = 1),

                             selectInput(ns("fontFam"), label = "Font Family",
                                         choices = c("Sans (Arial)" = "sans",
                                                     "Serif (Times New Roman)" = "serif",
                                                     "Mono (Courier New)" = "mono"),
                                         selected = "sans", multiple = FALSE)
                    ),

                    tabPanel(title = "Download:",

                             numericInput(inputId = ns("plotHt"), label = "Height", min = 2, max = 50,
                                          value = 15, step = .5),

                             numericInput(inputId = ns("plotWd"), label = "Width", min = 2, max = 50,
                                          value = 15, step = .5),

                             selectInput(inputId = ns("plotUnit"), label = "Units",
                                         choices = c("cm", "in", "mm"),
                                         selected = "cm", multiple = FALSE),

                             numericInput(inputId = ns("plotDPI"), label = "DPI", min = 75, max = 1000,
                                          value = 300, step = 25),

                             selectInput(inputId = ns("fileType"), label = "File Type",
                                         choices = c("tiff", "png", "eps", "ps", "tex", "pdf",
                                                     "jpeg", "bmp", "svg", "wmf"),
                                         selected = "tiff", multiple = FALSE),

                             downloadButton(outputId = ns("plotDwnld"), label = "Download Plot"),
                    )
        )


      ),

      mainPanel(
        textOutput(outputId = ns("warText")),

        ggiraph::girafeOutput(outputId = ns("plotOut"), height = 750)
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
#' @import shiny ggplot2 ggiraph
#' @import shiny ggiraph
#' @export
plotting_page <- function(input, output, session, descent_data)
{
  reacVals <- reactiveValues()

  # reacVals$data <- reactive(example_data)


  ### Modify Data for Cluster Names ###
  ### DELETE THIS FROM FINAL CODE ###
  reacVals$data <- reactive(modData(example_data, input$pathN, input$clustN))

  ### Update plotType Based on Data Size ###
  observe({
    pn <- nrow(reacVals$data())
    cn <- length(unique(reacVals$data()$clusterName))

    if (pn <= 50 & cn <= 10) {
      updateSelectInput(session, "plotType", choices = c("By Cluster" = "clust", "By Pathway" = "pth"), selected = "clust")

      } else if (pn > 50 | cn > 10){
        updateSelectInput(session, "plotType", choices = c("By Cluster" = "clust", "By Pathway (Unavailable)" = "long"), selected = "clust")

        }
  })

  ### Update lgdPosition based on coordFlip ###
  observe({
    if (!isTRUE(input$coordFlip)){
      updateSelectInput(session, "lgdPosition", choices = c("top", "bottom"), selected = "bottom")
    } else if (isTRUE(input$coordFlip)){
      updateSelectInput(session, "lgdPosition", choices = c("left", "right", "top", "bottom"), selected = "bottom")
    }
  })

  ### Create Plot ###
  reacVals$plotOut <- eventReactive(input$actPlot,
                                    switch(input$plotType,
                                           "pth" = {
                                             dat <- reactive(reacVals$data())

                                             pathwayGraph(ontoID = dat()$ontoID,
                                                          ontoTerm = dat()$ontoTerm,
                                                          pValue = dat()$pValue,
                                                          clusterNumber = dat()$clusterNumber,
                                                          clusterName = dat()$clusterName,
                                                          enrichmentScore = dat()$enrichmentScore,
                                                          direction = dat()$direction,
                                                          plotEnrichment = input$axisType,
                                                          coordFlip = input$coordFlip,
                                                          themeSet = input$themeSet,
                                                          colorSet = input$colorSet,
                                                          lgdPosition = input$lgdPosition,
                                                          nameSize = input$nameSize,
                                                          axTxtSize = input$axTxtSize,
                                                          axTitleSize = input$axTitleSize,
                                                          lgTxtSize = input$lgTxtSize,
                                                          lgTitleSize = input$lgTitleSize,
                                                          fontFam = input$fontFam)
                                             },

                                           "clust" = {
                                             dat <- reactive(reacVals$data())

                                             clusterGraph(ontoID = dat()$ontoID,
                                                          ontoTerm = dat()$ontoTerm,
                                                          pValue = dat()$pValue,
                                                          clusterNumber = dat()$clusterNumber,
                                                          clusterName = dat()$clusterName,
                                                          enrichmentScore = dat()$enrichmentScore,
                                                          direction = dat()$direction,
                                                          plotEnrichment = input$axisType,
                                                          coordFlip = input$coordFlip,
                                                          themeSet = input$themeSet,
                                                          colorSet = input$colorSet,
                                                          nameSize = input$nameSize,
                                                          axTxtSize = input$axTxtSize,
                                                          axTitleSize = input$axTitleSize,
                                                          fontFam = input$fontFam)
                                           },

                                           "long" = {NULL}
                                           )
                                    )

  ### Display Plot - as ggiraph svg ###
  observeEvent(input$actPlot, {
      h <- eventReactive(input$actPlot,{
        switch(input$plotUnit,
               "cm" = {input$plotHt * 0.393701},
               "in" = {input$plotHt},
               "mm" = {input$plotHt * 0.0393701})
        })

      w <- eventReactive(input$actPlot,{
        switch(input$plotUnit,
               "cm" = {input$plotWd * 0.393701},
               "in" = {input$plotWd},
               "mm" = {input$plotWd * 0.0393701})
      })

    output$plotOut <- ggiraph::renderGirafe(ggiraph::girafe(ggobj = reacVals$plotOut(),
                                                             width_svg = w(),
                                                             height_svg = h()))
    })

  ### Create Warning Message ###
  observeEvent(input$actPlot,
               switch(input$plotType,
                      "pth" = {output$warTest <- NULL},
                      "clust" = {output$warTest <- NULL},
                      "long" = {
                        output$warText <- renderText(
                          "Plotting enrichment by pathway is restricted to 50 or less pathways and 10 or less clusters.
                          Please use plot By Cluster to visualize a larger number of pathways and clusters."
                        )
                      }
                      ))



  ### Download Plot ###
  reacVals$plotDwnld <- reactive(reacVals$plotOut())
  reacVals$plotUnit <- reactive(switch(input$plotUnit, "cm" = "cm", "in" = "in", "mm" = "mm"))
  reacVals$plotHt <- reactive(input$plotHt)
  reacVals$plotWd <- reactive(input$plotWd)
  reacVals$plotDPI <- reactive(input$plotDPI)
  reacVals$fileType <- reactive(input$fileType)

  output$plotDwnld <- downloadHandler(
    filename = function() {paste("plot", reacVals$fileType(), sep = ".")},
    content = function(file) {
      ggplot2::ggsave(file, plot = reacVals$plotOut(), device = reacVals$fileType(),
                      width = reacVals$plotWd(), height = reacVals$plotHt(),
                      units = reacVals$plotUnit(), dpi = reacVals$plotDPI())
    }
  )

  ### Stop App on Session End ###
  session$onSessionEnded(function() {
    stopApp()
  })
}

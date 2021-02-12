#' UI for the plotting page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @import ggplot2 plotly RColorBrewer ggsci
#' @export
plotting_page_ui <- function(id)
{
  ns <- shiny::NS(id)

  fluidPage(
    titlePanel("Ontology Descent: Plotting Enrichment"),

    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = ns("plotType"), label = "Plot Type",
                     choices = c("By Cluster" = "clust", "By Pathway" = "pth"),
                     selected = "clust"),

        checkboxInput(inputId = ns("axisType"), label = "Plot enrichmentScore (replaces pValue)", value = FALSE),

        checkboxInput(inputId = ns("coordFlip"), label = "Flip X and Y axis", value = FALSE),

        br(),

        actionButton(inputId = ns("actPlot"), label = "Show plot"),

        br(),

        tabsetPanel(id = "Graphical Options",

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
                             ),

                    tabPanel(title = "Style:",
                             selectInput(inputId = ns("themeSet"), label = "Plot Theme",
                                         choices = c("bw", "classic", "grey", "minimal", "dark"),
                                         selected = "bw", multiple = FALSE),

                             selectInput(inputId = ns("colorSet"), label = "Color Palette",
                                         choices = c("Set1", "NPG", "AAAS", "NEJM", "Lancet", "JCO",
                                                     "UCSCGB", "D3", "IGV", "UChicago", "Futurama",
                                                     "RickAndMorty", "TheSimpsons"),
                                         selected = "Set1", multiple = FALSE),

                             selectInput(inputId = ns("lgdPosition"), label = "Legend Position",
                                         choices = c("right", "left", "top", "bottom"),
                                         selected = "right"),
                             ),

                    tabPanel(title = "Text:",
                             numericInput(ns("nameSize"), label = "Pathway (size)",
                                          value = 10, min = 4, max = 96, step = 1),

                             numericInput(ns("axTxtSize"), label = "Axis text (size)",
                                          value = 10, min = 4, max = 96, step = 1),

                             numericInput(ns("axTitleSize"), label = "Axis title (size)",
                                          value = 12, min = 4, max = 96, step = 1),

                             numericInput(ns("lgTxtSize"), label = "Legend text (size)",
                                          value = 10, min = 4, max = 96, step = 1),

                             numericInput(ns("lgTitleSize"), label = "Legend title (size)",
                                          value = 12, min = 4, max = 96, step = 1),

                             selectInput(ns("fontFam"), label = "Font Family",
                                         choices = c("Sans (Arial)" = "sans",
                                                     "Serif (Times New Roman)" = "serif",
                                                     "Mono (Courier New)" = "mono"),
                                         selected = "sans", multiple = FALSE)
                    )
        )


      ),

      mainPanel(
        plotOutput(outputId = ns("plotOut"), inline = TRUE),

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

  ### Get Plot ###
  reacVals$plotOut <- eventReactive(input$actPlot,
                                    switch(input$plotType,
                                           "pth" = {
                                             dat <- reactive({example_data[order(example_data$pValue)[1:50],]})

                                             pathwayGraph(ontoID = dat()$ontoID,
                                                          ontoTerm = dat()$ontoTerm,
                                                          pValue = dat()$pValue,
                                                          clusterNumber = dat()$clusterNumber,
                                                          clusterName = dat()$clusterName,
                                                          enrichmentScore = dat()$enrichmentScore,
                                                          direction = dat()$direction,
                                                          plotEnrichment = input$axisType,
                                                          coordFlip = input$coordFlip,
                                                          interactive = FALSE,
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
                                             dat <- reactive({example_data})

                                             clusterGraph(ontoID = dat()$ontoID,
                                                          ontoTerm = dat()$ontoTerm,
                                                          pValue = dat()$pValue,
                                                          clusterNumber = dat()$clusterNumber,
                                                          clusterName = dat()$clusterName,
                                                          enrichmentScore = dat()$enrichmentScore,
                                                          direction = dat()$direction,
                                                          plotEnrichment = input$axisType,
                                                          coordFlip = input$coordFlip,
                                                          interactive = FALSE,
                                                          themeSet = input$themeSet,
                                                          colorSet = input$colorSet,
                                                          lgdPosition = input$lgdPosition,
                                                          nameSize = input$nameSize,
                                                          axTxtSize = input$axTxtSize,
                                                          axTitleSize = input$axTitleSize,
                                                          lgTxtSize = input$lgTxtSize,
                                                          lgTitleSize = input$lgTitleSize,
                                                          fontFam = input$fontFam)
                                             }
                                           )
                                    )

  ### Display Plot ###
  observeEvent(input$actPlot,{
    h <- eventReactive(input$actPlot,{
      switch(input$plotUnit,
             "cm" = {input$plotHt * 37.795275591},
             "in" = {input$plotHt * 96},
             "mm" = {input$plotHt * 3.7795275591})
      })

    w <- eventReactive(input$actPlot,{
      switch(input$plotUnit,
             "cm" = {input$plotWd * 37.795275591},
             "in" = {input$plotWd * 96},
             "mm" = {input$plotWd * 3.7795275591})
    })

    output$plotOut <- renderImage({

      outfile <- tempfile(fileext = ".png")

      png(outfile, width = w(), height = h())
      print(reacVals$plotOut())
      dev.off()

      list(src = outfile,
           contentType = "image/png",
           width = w(),
           height = h(),
           alt = "Plot PNG")
      }, deleteFile = TRUE)
  }
  )

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
}

#' UI for the plotting page
#'
#' @param id unique id for this module
#'
#' @import shiny ggiraph shinyWidgets xlsx
#' @export
plotting_page_ui <- function(id)
{
  ns <- shiny::NS(id)

  fluidPage(
    tags$head(tags$style("
                       .jhr{
                       display: inline;
                       padding-left: 10px;
                       }")),

    titlePanel("Ontology Descent: Plotting Enrichment"),

    sidebarLayout(
      sidebarPanel(

        selectInput(inputId = ns("plotType"), label = "Plot Type:", choices = NULL, selected = NULL, multiple = FALSE),

        checkboxInput(inputId = ns("axisType"), label = "Plot enrichmentScore (replaces pValue)", value = FALSE),

        actionButton(inputId = ns("actPlot"), label = "Show plot"),

        downloadButton(outputId = ns("dataDwnld"), label = "Download Data"),

        downloadButton(outputId = ns("plotDwnld"), label = "Download Plot"),

        br(),
        br(),

        shinyWidgets::pickerInput(inputId = ns("themeSet"), label = "Plot Theme",
                                  choices = c("bw", "classic", "grey", "minimal", "dark", "linedraw"),
                                  selected = "minimal", multiple = FALSE,
                                  choicesOpt = list(content = themeIcon())),

        sliderInput(ns("dotSize"), label = "Dot Size",
                    value = 1, min = 0, max = 5, step = .5),

        uiOutput(ns("lgdPosition")),

        actionButton(inputId = ns("upDate1"), label = "Refresh Plot"),

        br(),
        br(),

        shinyWidgets::dropdownButton(
          h3("Text Options:"),

          br(),

          selectInput(ns("fontFam"), label = "Font Family",
                      choices = c("Sans (Arial)" = "sans",
                                  "Serif (Times New Roman)" = "serif",
                                  "Mono (Courier New)" = "mono"),
                      selected = "sans", multiple = FALSE),

          sliderInput(ns("axTitleSize"), label = "Axis title (size)",
                      value = 9, min = 4, max = 48, step = 1),

          sliderInput(ns("nameSize"), label = "Pathway Names (size)",
                       value = 7, min = 4, max = 48, step = 1),

          sliderInput(ns("axTxtSize"), label = "Axis text (size)",
                       value = 7, min = 4, max = 48, step = 1),

          uiOutput(ns("lgTitleSize")),

          uiOutput(ns("lgTxtSize")),

          br(),

          actionButton(inputId = ns("upDate2"), label = "Refresh Plot"),

          br(),

          circle = FALSE, up = TRUE, status = "info", label = "Text Options", width = "300px",
          tooltip = tooltipOptions(title = "Click to modify text")
        ),

        br(),

        shinyWidgets::dropdownButton(
          h3("Download Options:"),

          br(),

          numericInput(inputId = ns("plotHt"), label = "Plot Height", min = 2, max = 50,
                       value = 15, step = .5),

          numericInput(inputId = ns("plotWd"), label = "Plot Width", min = 2, max = 50,
                       value = 15, step = .5),

          selectInput(inputId = ns("plotUnit"), label = "Size Units",
                      choices = c("cm", "in", "mm"),
                      selected = "cm", multiple = FALSE),

          numericInput(inputId = ns("dwnDPI"), label = "DPI", min = 75, max = 1000,
                       value = 300, step = 25),

          selectInput(inputId = ns("fileType"), label = "File Type",
                      choices = c("tiff", "png", "eps", "ps", "tex", "pdf",
                                  "jpeg", "bmp", "svg", "wmf"),
                      selected = "tiff", multiple = FALSE),

          br(),

          actionButton(inputId = ns("upDate3"), label = "Refresh Plot"),

          br(),

          circle = FALSE, up = TRUE, status = "info", label = "Download / Plot Size Options", width = "300px",
          tooltip = tooltipOptions(title = "Click to modify plot size"))


        ),

      mainPanel(
        uiOutput(ns("emptyWar")),
        ggiraph::girafeOutput(outputId = ns("plotOut"), width = "100%", height = 750)
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
#' @import shiny ggiraph
#' @export
plotting_page <- function(input, output, session, descent_data)
{
  ns <- session$ns

  reacVals <- reactiveValues()

  reacVals$data <- reactive({
    req(descent_data$inputData)
    return(descent_data$inputData)
  })

  ### Data NOT present and clustered ###
  output$emptyPlot <- ggiraph::renderGirafe(ggiraph::girafe(ggobj = errorMessage("empty"),
                                                            options = list(
                                                              ggiraph::opts_toolbar(saveaspng = FALSE)
                                                            )))
  observe({
    if (is.null(descent_data$inputData$clusterTerm)){
      output$emptyWar <- renderUI({
        ggiraph::girafeOutput(outputId = ns("emptyPlot"), width = "100%", height = 750)
        })
    } else if (!is.null(descent_data$inputData$clusterTerm)){
      output$emptyWar <- NULL
    }
  })

  ### Render UI Based on Selected Options ###

  ### Update plotType Based on Data Size ###
  observe({
    req(reacVals$data())
    pn <- nrow(reacVals$data())
    cn <- length(unique(reacVals$data()$clusterTerm))

    if (pn <= 50 & cn <= 10) {
      updateSelectInput(session, "plotType", choices = c("By Cluster" = "clust", "By Pathway" = "pth"), selected = "clust")

    } else if (pn > 50 | cn > 10){
      updateSelectInput(session, "plotType", choices = c("By Cluster" = "clust", "By Pathway (Unavailable)" = "long"), selected = "clust")
    }
  })

  ### Plot Type Based Options - Dot Size / Dot Shape / Legend Position / Legend Text ###
  observe(switch(input$plotType,
                 "pth" = {
                   updateNumericInput(session, "dotSize", label = "Dot Size", value = 2, min = 0, max = 5, step = .25)

                   output$lgdPosition <- renderUI(
                     selectInput(ns("lgdPosition"), label = "Legend Position", choices = c("top", "bottom"), selected = "bottom")
                   )

                   output$lgTitleSize <- renderUI(
                     sliderInput(ns("lgTitleSize"), label = "Legend title (size)", value = 9, min = 4, max = 48, step = 1)
                   )

                   output$lgTxtSize <- renderUI(
                     sliderInput(ns("lgTxtSize"), label = "Legend text (size)", value = 7, min = 4, max = 48, step = 1)
                   )
                 },

                 "clust" = {
                   updateNumericInput(session, "dotSize", label = "Dot Size", value = 1, min = 0, max = 5, step = .25)

                   output$lgdPosition <- NULL

                   output$lgTitleSize <- NULL

                   output$lgTxtSize <- NULL
                 }))


  ### Create Plot ###
  reacVals$plotOut <- eventReactive(input$actPlot | input$upDate1 | input$upDate2 | input$upDate3,
                                    switch(input$plotType,
                                           "pth" = {
                                             req(reacVals$data()$clusterTerm)

                                             dat <- reactive(reacVals$data())

                                             pathwayGraph(ontoID = dat()$ontoID,
                                                          ontoTerm = cutText(dat()$ontoTerm, 52),
                                                          pValue = dat()$pValue,
                                                          clusterNumber = dat()$clusterNumber,
                                                          clusterName = cutText(dat()$clusterTerm, 52),
                                                          enrichmentScore = dat()$enrichmentScore,
                                                          direction = dat()$direction,
                                                          colorManual = dat()$color,
                                                          plotEnrichment = input$axisType,
                                                          dotSize = input$dotSize,
                                                          themeSet = input$themeSet,
                                                          lgdPosition = input$lgdPosition,
                                                          nameSize = input$nameSize,
                                                          axTxtSize = input$axTxtSize,
                                                          axTitleSize = input$axTitleSize,
                                                          lgTxtSize = input$lgTxtSize,
                                                          lgTitleSize = input$lgTitleSize,
                                                          fontFam = input$fontFam)
                                           },

                                           "clust" = {
                                             req(reacVals$data()$clusterTerm)

                                             dat <- reactive(reacVals$data())

                                             clusterGraph(ontoID = dat()$ontoID,
                                                          ontoTerm = cutText(dat()$ontoTerm, 52),
                                                          pValue = dat()$pValue,
                                                          clusterNumber = dat()$clusterNumber,
                                                          clusterName = cutText(dat()$clusterTerm, 52),
                                                          enrichmentScore = dat()$enrichmentScore,
                                                          direction = dat()$direction,
                                                          colorManual = dat()$color,
                                                          plotEnrichment = input$axisType,
                                                          dotSize = input$dotSize,
                                                          themeSet = input$themeSet,
                                                          nameSize = input$nameSize,
                                                          axTxtSize = input$axTxtSize,
                                                          axTitleSize = input$axTitleSize,
                                                          fontFam = input$fontFam)
                                           },

                                           "long" = {errorMessage("long")}
                                    )
  )


  observeEvent(input$actPlot, {
    req(reacVals$plotOut())

    h <- eventReactive(input$actPlot | input$upDate1 | input$upDate2 | input$upDate3,{
      switch(input$plotUnit,
             "cm" = {input$plotHt * 0.393701},
             "in" = {input$plotHt},
             "mm" = {input$plotHt * 0.0393701})
    })

    w <- eventReactive(input$actPlot | input$upDate1 | input$upDate2 | input$upDate3,{
      switch(input$plotUnit,
             "cm" = {input$plotWd * 0.393701},
             "in" = {input$plotWd},
             "mm" = {input$plotWd * 0.0393701})
    })

    output$plotOut <- ggiraph::renderGirafe(ggiraph::girafe(ggobj = reacVals$plotOut(),
                                                            width_svg = w(),
                                                            height_svg = h(),
                                                            options = list(
                                                              ggiraph::opts_selection(type = "single"),
                                                              ggiraph::opts_hover(css = "fill:wheat;stroke:orange;"),
                                                              ggiraph::opts_zoom(min = 1, max = 5),
                                                              ggiraph::opts_toolbar(saveaspng = FALSE)
                                                            )))
  })

  ### Download Plot ###
  reacVals$plotDwnld <- reactive(reacVals$plotOut())
  reacVals$plotUnit <- reactive(switch(input$plotUnit, "cm" = "cm", "in" = "in", "mm" = "mm"))
  reacVals$plotHt <- reactive(input$plotHt)
  reacVals$plotWd <- reactive(input$plotWd)
  reacVals$plotDPI <- reactive(input$dwnDPI)
  reacVals$fileType <- reactive(input$fileType)

  output$plotDwnld <- downloadHandler(
    filename = function() {paste("plot", reacVals$fileType(), sep = ".")},
    content = function(file) {
      ggplot2::ggsave(file, plot = reacVals$plotOut(), device = reacVals$fileType(),
                      width = reacVals$plotWd(), height = reacVals$plotHt(),
                      units = reacVals$plotUnit(), dpi = reacVals$plotDPI())
    }
  )


  ### Download Data ###
  output$dataDwnld <- downloadHandler(
    filename = function() {"OntoDescResults.xlsx"},
    content = function(file) {xlsx::write.xlsx(reorderData(reacVals$data()), file = file, sheetName = "OntoDesc",
                                               row.names = FALSE, col.names = TRUE)}
  )

  ### Stop App on Session End ###
  session$onSessionEnded(function() {
    stopApp()
  })
}

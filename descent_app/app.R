library(shiny)
library(DT)
library(tidyverse)

ui <- fluidPage(
  titlePanel(
    "Ontology Descent: A data visualization tool for Gene Ontology Enrichment data"
  ),
  tabsetPanel(type = "tabs",
              tabPanel("Data Entry",
  fluidRow(
    column(
      2,
      textAreaInput(
        "Terms",
        h4("GO-terms"),
        value = "Enter a list of GO-terms (ex. GO:0051004)",
        height = "100%",
        rows = 10,
        resize = "both"
      ),
      actionButton("Setting1", label = "Submit!")
    ),
    column(
      2,
      textAreaInput(
        "Enrichment",
        h4("Enrichment Scores"),
        value = "Enrichment Scores",
        height = "100%",
        rows = 10,
        resize = "both"
      )
    ),
    column(
      2,
      textAreaInput(
        "P_values",
        h4("Point Size"),
        value = "Enter matching P-values",
        height = "100%",
        rows = 10,
        resize = "both"
      )
    ),
    column(
      2,
           textAreaInput(
             "Direction",
             h4("Enrichment Direction"),
             value = "Enrichment direction (up or down)",
             height = "100%",
             rows = 10,
             resize = "both")
  ),
  column(2, selectInput(
    "dummy",
    label = "Enable P-hacking?",
    choices = c("Yes!", "Hell no!")
  ),selectInput(
    "dummy_2",
    label = "Bless with Leo's beard??",
    choices = c("Yes!", "Hell no!")
  ),
  selectInput(
    "dummy_3",
    label = "Do you want a banana?",
    choices = c("Yes!", "Hell no!")
  )
  )
  ),
  dataTableOutput(outputId = "GO_table")),
  tabPanel("Data Trimming", ""),
  tabPanel("Data Visualization", "")))




server <- function(input,output){
  data <- reactiveValues(GO_terms = NA,
                         Enrichment = NA,
                         P_values = NA,
                         Direction = NA)

     observeEvent(input$Setting1, {
       data$GO_terms <- strsplit(input$Terms, "\n")
       data$Enrichment <- strsplit(input$Enrichment, "\n")
       data$P_values <- strsplit(input$P_values, "\n")
       data$Direction <- strsplit(input$Direction, "\n")
       data_matrix<-data.frame(data$GO_terms, data$Enrichment, data$P_values, data$Direction)
       colnames(data_matrix) <- c("GO Terms", "Enrichment", "P-values", "Direction")
       output$GO_table <- renderDataTable(data_matrix)
       #add function that returns error if uneven length of our four factors are returned
       })


}


shinyApp(ui = ui, server = server)

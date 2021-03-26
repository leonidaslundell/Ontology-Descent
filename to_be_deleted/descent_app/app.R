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
        h4("Ontology Terms"),
        value = "Enter a list of ontology terms (ex. GO:0051004)",
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
        "pValues",
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
  data <- reactiveValues(InputData = data.frame(
    GO_terms = NA,
    Enrichment = NA,
    pValues = NA,
    Direction = NA
  ))
  #change pValue to Leo's stupid annotation

     observeEvent(input$Setting1, {
       data_matrix<- data.frame(unlist(strsplit(input$Terms, "\n")),
                                unlist(strsplit(input$Enrichment, "\n")),
                                unlist(strsplit(input$pValues, "\n")),
                                unlist(strsplit(input$Direction, "\n")))
       colnames(data_matrix) <- c("GO Terms", "Enrichment", "P-values", "Direction")
       data$InputData<- data_matrix
       output$GO_table <- renderDataTable(data$InputData)
       #}
       #add function that returns error if uneven length of our four factors are returned
       })


}


shinyApp(ui = ui, server = server)

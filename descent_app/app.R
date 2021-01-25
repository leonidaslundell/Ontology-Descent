library(shiny)

ui <- fluidPage(
  titlePanel(
    "Ontology Descent: A data visualization tool for Gene Ontology Enrichment data"
  ),
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
  ))




server <- function(input,output){}


shinyApp(ui = ui, server = server)

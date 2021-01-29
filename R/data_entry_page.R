#' UI for the data page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
data_entry_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  fluidPage(
    titlePanel(
      "Ontology Descent: A data visualization tool for Gene Ontology Enrichment data"
    ),

    fluidRow(
      column(
        2,
        textAreaInput(
          ns("OntoID"),
          h4("Ontology ID"),
          value = "Ontology ID",
          height = "100%",
          rows = 10,
          resize = "both"
        ),
        actionButton(ns("Setting1"), label = "Submit!")
      ),
      column(
        2,
        textAreaInput(
          ns("Terms"),
          h4("Ontology Terms"),
          value = "Ontology Terms",
          height = "100%",
          rows = 10,
          resize = "both"
        )),
      column(
        2,
        textAreaInput(
          ns("Enrichment"),
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
          ns("pValues"),
          h4("Point Size"),
          value = "Direction",
          height = "100%",
          rows = 10,
          resize = "both"
        )
      ),
      column(
        2,
        textAreaInput(
          ns("Direction"),
          h4("Enrichment Direction"),
          value = "Enrichment direction (up or down)",
          height = "100%",
          rows = 10,
          resize = "both"
        )
      ),
      column(
        2,
        selectInput(
          ns("dummy"),
          label = "Enable P-hacking?",
          choices = c("Yes!", "Hell no!")
        ),
        selectInput(
          ns("dummy_2"),
          label = "Bless with Leo's beard??",
          choices = c("Yes!", "Hell no!")
        ),
        selectInput(
          ns("dummy_3"),
          label = "Do you want a banana?",
          choices = c("Yes!", "Hell no!")
        )
      )
    ),
    dataTableOutput(outputId = ns("GO_table"))
  )
}

#' Server for the data page
#'
#' @param input shiny parameter
#' @param output shiny parameter
#' @param session shiny parameter
#' @param descent_data reactiveValues, contains gene ontology data
#'
#' @import shiny
#' @export
data_entry_page <- function(input, output, session, descent_data)
{
   data <- descent_data
   output$GO_table <- renderDataTable(data$inputData)
   observeEvent(input$Setting1, {
     data_matrix<- data.frame(unlist(strsplit(input$OntoID, "\n")),
                              unlist(strsplit(input$Terms, "\n")),
                              unlist(strsplit(input$Enrichment, "\n")),
                              unlist(strsplit(input$pValues, "\n")),
                              unlist(strsplit(input$Direction, "\n")))
     colnames(data_matrix) <- c("ontoID", "ontoTerm", "Enrichment", "P-values", "Direction")
     data$InputData<- data_matrix
     output$GO_table <- renderDataTable(data$InputData)
    #}
    #add function that returns error if uneven length of our four factors are returned
  })
}

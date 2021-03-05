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
        actionButton(ns("submitButton"), label = "Submit!"),
        actionButton(ns("dummy"), label = "Load dummy data")
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
        )
      ),
      column(
        2,
        textAreaInput(
          ns("Enrichment"),
          h4("Enrichment"),
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
          value = "pValue",
          height = "100%",
          rows = 10,
          resize = "both"
        )
      ),
      column(
        2,
        textAreaInput(
          ns("Direction"),
          h4("Direction"),
          value = "Enrichment direction (up or down)",
          height = "100%",
          rows = 10,
          resize = "both"
        )
      ),
      column(
        2,
        fileInput(
          inputId = ns("file"),
          label = "Upload a csv or xlsx file",
          buttonLabel = "Upload",
          multiple = F,
          accept = c(".csv", ".xlsx")
        ),
        radioButtons(
          inputId = ns("sep"),
          label = "Seperator",
          choices = c(
            Auto = "auto",
            Comma = ",",
            Semicolon = ":",
            Tab = "\t"
          )
        ),
        helpText(
          "A csv file should contain ontology ID, ontology term, enrichment value, p.value and direction, in that order"
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
  dummy_ref <- descent_data
  imported_values <- NULL
  #Load data input from file upload
  observeEvent(
    input$file,
    { if(stringr::str_ends(input$file$datapath, "\\.csv")){
      imported_values <-
        data.table::fread(input$file$datapath, sep = input$sep)
      col_names <- c("ontoID",
                     "ontoTerm",
                     "Enrichment",
                     "P-values",
                     "Direction")
    for(i in 1:5){
      colnames(imported_values)[i] <- col_names[i]
    }
    values <-
      reactive({
        validate(need(
          length(imported_values) == 5,
          "Your uploaded file is not the correct format"
        ))
        imported_values
      })
    output$GO_table <- renderDataTable(values())
    descent_data$inputData <- values()

    }
      else if(stringr::str_ends(input$file$datapath, "\\.xlsx")){
       imported_values <- openxlsx::read.xlsx(input$file$datapath)
       col_names <- c("ontoID",
                      "ontoTerm",
                      "Enrichment",
                      "P-values",
                      "Direction")
       for(i in 1:5){
         colnames(imported_values)[i] <- col_names[i]
       }
       values <-
         reactive({
           validate(need(
             length(imported_values) == 5,
             "Your uploaded file is not the correct format"
           ))
           imported_values
         })
       output$GO_table <- renderDataTable(values())
       descent_data$inputData <- values()
      }
      else{
        showNotification("Your data is not the right file type")
      }
  })
#input from input columns
  observeEvent(input$submitButton,
               {data_matrix <-
                 data.frame(
                   "ontoID" = if(is.na(input$OntoID)) " " else unlist(strsplit(input$OntoID, "\n")),
                   "ontoTerm" = if(is.na(input$Terms)) " " else unlist(strsplit(input$Terms, "\n")),
                   "Enrichment" = if(is.na(input$Enrichment)) " " else unlist(strsplit(
                     input$Enrichment, "\n"
                   )),
                   "P-values" = if(is.na(input$pValues)) " " else unlist(strsplit(input$pValues, "\n")),
                   "Direction" = if(is.na(input$Direction)) " " else unlist(strsplit(
                     input$Direction, "\n"
                   ))
                 )

                 data$inputData <- data_matrix
                 descent_data$inputData <- data$inputData
                 output$GO_table <- renderDataTable(descent_data$inputData)
               })
#load dummy data
  observeEvent(input$dummy,
               {
                 dummy_ref <- get_test_data()
                 dummy_data <- dummy_ref$inputData
                 data_matrix <-
                   data.frame(
                     dummy_data$ontoID,
                     dummy_data$ontoTerm,
                     dummy_data$enrichmentScore,
                     dummy_data$pValue,
                     dummy_data$direction
                   )
                 colnames(data_matrix) <-
                   c("ontoID",
                     "ontoTerm",
                     "Enrichment",
                     "P-values",
                     "Direction")
                 descent_data$inputData <- data_matrix
                 output$GO_table <- renderDataTable(descent_data$inputData)
               })


}

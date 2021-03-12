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
        6,
        textAreaInput(
          ns("data_entry"),
          h4("Paste in data, with heads in first row, select delimiter from the menu"),
          value = "Copy in data",
          height = "100%",
          rows = 10,
          resize = "both"
        ),
        actionButton(ns("submitButton"), label = "Submit!"),
        actionButton(ns("dummy"), label = "Load dummy data")
      ),
      column(
        4,
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
            Comma = ",",
            Semicolon = ":",
            Tab = "\t",
            Auto = "auto"
          )
        ),
        helpText(
          "A csv file should contain ontology ID, ontology term, enrichment value, p.value and direction, in that order"
        ),
        img(src = "logo.png", heigth = 120, width = 200)

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
                   "data" =  unlist(strsplit(input$data_entry, "\n"))
                 )
               data_matrix <- data.frame(do.call("rbind", strsplit(data_matrix$data, input$sep, fixed = T)))
               colnames(data_matrix) <- data_matrix[1,]
               data_matrix <- data_matrix[-1,]

                 data$inputData <- data_matrix
                 descent_data$inputData <- data$inputData
                 output$GO_table <- renderDataTable(descent_data$inputData)

                 #### Adding the clustereR function now, but at some point need to add an evaluator before running clustereR.
                 #### otherwise its gonna be an ugly crash
                 temp <- clustereR(net, GOnames, GOlength, descent_data$inputData)

                 descent_data <- reactiveValues(inputData = merge(descent_data$inputData[,c("ontoID", "enrichmentScore", "pValue", "direction")],
                                                                  temp$res, by = "ontoID", all.y = T),
                                                netPlot = temp$plot)
               })
#load dummy data
  observeEvent(input$dummy,
               {
                 dummy_ref <- get_test_data()
                 descent_data$inputData <- dummy_ref$inputData

                  output$GO_table <- renderDataTable(descent_data$inputData)
               })


}

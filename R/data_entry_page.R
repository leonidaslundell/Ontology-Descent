#' UI for the data page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
data_entry_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  shinyWidgets::useSweetAlert()

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
        actionButton(ns("Setting1"), label = "Submit!"),
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
          "Your entry file needs to have a first column named 'ontoID' (ex. GO:00010) and a second column called 'pValue' (ex. 3E-5). Beware of case in names! "
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
        data.table::fread(input$file$datapath, sep = "auto")
      if(colnames(imported_values)[1] == "ontoID" & colnames(imported_values)[2] == "pValue"){
        output$GO_table <- renderDataTable(imported_values)
        descent_data$inputData <- imported_values
      }
      else{
        shinyWidgets::sendSweetAlert(session = session,
                                     title = "Input Error",
                                     text = "Your table is not the correct format. \n
                                     please name the first column 'ontoID' and the second column 'pValue'",
                                     type = "error")
      }}






      else if(stringr::str_ends(input$file$datapath, "\\.xlsx")){
       imported_values <- openxlsx::read.xlsx(input$file$datapath)

       if(colnames(imported_values)[1] == "ontoID" & colnames(imported_values)[2] == "pValue"){
         output$GO_table <- renderDataTable(imported_values)
         descent_data$inputData <- imported_values
       }
       else{
         shinyWidgets::sendSweetAlert(session = session,
                                      title = "Input Error",
                                      text = "Your table is not the correct format. \n
                                     please name the first column 'ontoID' and the second column 'pValue'",
                                     type = "error")
       }}
      else{
        shinyWidgets::sendSweetAlert(session = session,
                                     title = "Input Error",
                                     text = "Your file is not the right format. \n Supported formats are xlsx and csv",
                                     type = "error")
      }
  })
#input from input columns
  observeEvent(input$Setting1,
               {if(nchar(input$data_entry)>0){

                 data_matrix <- try(data.table::fread(input$data_entry, header = T))
                 if(all(class(data_matrix) == "try-error")){shinyWidgets::sendSweetAlert(session = session,
                                                                                    title = "Input Error",
                                                                                    text = "Check your seperators, number of columns or incomplete rows",
                                     type = "error")}
              else{
               if(colnames(data_matrix)[1] == "ontoID" &
                  colnames(data_matrix)[2] == "pValue"){
                 output$GO_table <- renderDataTable(data_matrix)
                 descent_data$inputData <- data_matrix

               }
               else{
                 shinyWidgets::sendSweetAlert(session = session,
                                              title = "Input Error",
                                              text = "Your table is not the correct format. \n
                                     please name the first column 'ontoID' and the second column 'pValue'. \n
                                     Alternatively, check you have selected the correct seperator",
                                     type = "error")}

                                }}})
#load dummy data
  observeEvent(input$dummy,
               {
                 dummy_ref <- get_test_data()
                 descent_data$inputData <- dummy_ref$inputData

                  output$GO_table <- renderDataTable(descent_data$inputData)
               })


}

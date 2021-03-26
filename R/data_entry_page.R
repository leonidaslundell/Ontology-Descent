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
          h4("Paste in data, with headers in first row"),
          placeholder = "Copy in data",
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
  input_data <- reactiveValues(read_now = 0)

  # Observe if file is uploaded
  observeEvent(input$file, {
      input_data$x <- input$file$datapath
      input_data$type <- tools::file_ext(input$file$datapath)
      input_data$read_now <- input_data$read_now + 1 # To trigger loading of data
    })

  # Observe if text is submitted
  observeEvent(input$Setting1, {
    input_data$x <- input$data_entry
    input_data$type <- "text"
    input_data$read_now <- input_data$read_now + 1 # To trigger loading of data
  })

  # Attempt to read data
  observeEvent(input_data$read_now, {
    req(input_data$x) # To not crash at startup

    # The idea is to continuously run tests and add failures to error_messages.
    # If at the end error_messages is still of length 0, everything seems to have worked.
    error_message <- character(0)

    error_message <- c(error_message, validate_input(input_data$x, input_data$type))

    if (length(error_message) == 0) { #No errors in input
      imported_object <- tryCatch(read_input(input_data$x, input_data$type), error = function(e) e)

      # imported_values can either be an error from reading the file/text or properly read data
      if (inherits(imported_object, "error")) { # Failed to read data
        error_message <- c(error_message, imported_object$message)
      } else { # Properly read data
        imported_values <- imported_object
        error_message <- c(error_message, validate_data(imported_values, input_data$type))
      }
    }

    # If no error messages are present data was read properly and passed all test.
    if (length(error_message) > 0) {
      error_string <- paste(error_message, collapse = "\n")
      shinyWidgets::sendSweetAlert(session = session,
                                   title = "Input Error",
                                   text = error_string,
                                   type = "error")
    } else {
      output$GO_table <- renderDataTable(imported_values)
      descent_data$inputData <- imported_values
    }
  })

  #load dummy data
  observeEvent(input$dummy,
               {
                 dummy_ref <- get_test_data()
                 shiny::updateTextAreaInput(inputId = "data_entry",
                                            value = dummy_ref
                 )
               })


}

#' Validate input
#'
#' @param x character, input text string (file path or raw data)
#' @param type character, type of input. Determines which tests to run and phrasing of errors
#'
#' @return string with error messages (length 0 if no errors occured)
validate_input <- function(x, type) {
  supported_filetypes <- c("csv", "xlsx", "text")

  error_message <- character(0)

  if (!(type %in% supported_filetypes)) {
    error_message <- c(error_message, "Your file is not the right format. \n Supported formats are xlsx and csv")
  }

  if (type == "text") {
    # Checks specific for text input
    if (nchar(x) == 0) {
      error_message <- c(error_message, "No data in text box")
    }
  }
  error_message
}

#' Data reader
#'
#' @param x character, data to be read
#' @param type character, type of input. Determines which function to read.
#'
#' @return
#' @export
#'
#' @examples
read_input <- function(x, type) {
  if (type == "xlsx") {
    out <- openxlsx::read.xlsx(xlsxFile = x, colNames = TRUE)
  } else if (type == "csv") {
    out <- data.table::fread(file = x, header = TRUE)
  } else if (type == "text") {
    out <- data.table::fread(text = x, header = TRUE)
  } else {
    stop("type must be one of csv, xlsx or text")
  }
  out
}

#' Validate input data
#'
#' @param x input data object (must inherit from data.frame)
#' @param type character, type of input. Determines which tests to run and phrasing of errors
#'
#' @return string with error messages (length 0 if no errors occured)
validate_data <- function(x, type) {
  error_message <- character(0)

  # Less than two columns
  if (ncol(x) == 1) {
    error_message <- c(error_message, "Only one column detected.")
    if (type == "csv") {
      error_message <- c(error_message, "Please make sure fields are seperated by one of: comma, tab, space, pipe, colon or semicolon.")
    } else  if (type == "xlsx"){
      error_message <- c(error_message, "Please check excel file for errors.")
    } else {
      error_message <- c(error_message, "Please make sure pasted fields are seperated by one of: comma, tab, space, pipe, colon or semicolon.")
    }
    return(error_message) # We cant really test anything else if data was not parsed correctly
  }

  # Wrong column names
  if ((colnames(x)[1] != "ontoID") |
      (colnames(x)[2] != "pValue")) {
    error_message <- c(error_message, "Please name the first column 'ontoID' and the second column 'pValue'.")
  }

  # P-values not parsed correctly
  if (("pValues" %in% colnames(x)) && (class(x[["pValue"]]) != "numeric")) {
    error_message <- c(error_message, "pValue column not recognized as numeric. Typically the decimal seperated needs to be changed from a period to a comma or vice versa.")
  }
  error_message
}

#' UI for the clustering page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
clustering_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  fluidPage( # flexible layout function

    # Title
    titlePanel("Minimal application"),

    sidebarLayout(  # standard inputs on sidebar, outputs in main area layout
      sidebarPanel( # sidebar configuration
        textInput(inputId = ns("comment"),      # this is the name of the
                  # variable- this will be passed to server.R
                  label = "Say something?", # display label for the variable
                  value = ""                # initial value

        )),

      # Show a plot of the generated distribution
      mainPanel( # main output configuration
        h3("This is you saying it"), # title with HTML helper
        textOutput(ns("textDisplay"))    # this is the name of the output
        # element as defined in server.R
      )
    )
  )
}

#' Server for the clustering page
#'
#' @param input shiny parameter
#' @param output shiny parameter
#' @param session shiny parameter
#' @param descent_data reactiveValues, contains gene ontology data
#'
#' @import shiny
#' @export
clustering_page <- function(input, output, session, descent_data)
{
  browser()
  output$textDisplay <- renderText({ # mark function as reactive
    # and assign to output$textDisplay for passing to ui.R

    paste0("You said '", input$comment,           # from the text
           "'. There are ", nchar(input$comment), # input control as
           " characters in this.")                # defined in ui.R

  })
}

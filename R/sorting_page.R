#' UI for the sorting page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
sorting_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  #fluidPage( # flexible layout function
    sidebarLayout(sidebarPanel = sidebarPanel(checkboxGroupInput(ns("shown_groups"),
                                                    label = "Select groups to show",
                                                    choices = "No clusters defined yet"),
                                              width = 2),
                  mainPanel = mainPanel(uiOutput(ns("sorting_boxes")))
    )
  #)
}

#' Server for the sorting page
#'
#' @param input shiny parameter
#' @param output shiny parameter
#' @param session shiny parameter
#' @param descent_data reactiveValues, contains gene ontology data
#'
#' @import shiny
#' @importFrom sortable bucket_list
#' @export
sorting_page <- function(input, output, session, descent_data)
{
  ns <- session$ns
  observe(req(descent_data$inputData$clusterNumber,
              descent_data$inputData$clusterName))

  observeEvent(descent_data$active_page, {
    if (descent_data$active_page == "Sorting") { #No need to edit anything if the page is not active
      updateCheckboxGroupInput(session = session,
                               inputId = "shown_groups",
                               choices = unique(descent_data$inputData$clusterName))


    }
  })

  output$sorting_boxes <- renderUI({
    req(input$shown_groups)

    boxes_list <- list(header = NULL,
                       group_name = "bucket_list_group",
                       orientation = "horizontal")

    for (i in input$shown_groups) {
      idx_in_cluster <- descent_data$inputData$clusterName == i
      ontologies_in_cluster <- descent_data$inputData$ontoTerm[idx_in_cluster]
      this_ranked_list <- sortable::add_rank_list(
        text = i,
        ##################################################################
        #################### FIND BETTER SOLUTION THAN SELECTING 100 GENES
        ##################################################################
        labels = as.list(ontologies_in_cluster[1:100]),
        input_id = ns(i),
        options = sortable_options(multiDrag = TRUE)
      )
      boxes_list <- append(boxes_list, list(this_ranked_list))
    }
    do.call(what = "bucket_list", boxes_list)
  })

  observe({
    for (i in input$shown_groups){
      idx_in_cluster <- descent_data$inputData$ontoTerm %in% input[[i]]
      descent_data$inputData$clusterName[idx_in_cluster] <- i
    }
  })
}

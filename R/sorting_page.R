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
                                            actionButton(inputId = ns("move"), label = "Redefine clusters"),
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
              descent_data$inputData$clusterTerm))

  observeEvent(descent_data$clustered, {
    if (descent_data$clustered$exists) { #No need to edit anything if the page is not active
      updateCheckboxGroupInput(session = session,
                               inputId = "shown_groups",
                               choices = c(sort(unique(descent_data$inputData$clusterTerm)),
                                           "create new cluster"))


    }
  })

  output$sorting_boxes <- renderUI({
    req(input$shown_groups)

    boxes_list <- list(header = NULL,
                       group_name = "bucket_list_group",
                       orientation = "horizontal")

    for (i in input$shown_groups) {
      idx_in_cluster <- descent_data$inputData$clusterTerm == i
      ontologies_in_cluster <- descent_data$inputData$ontoTerm[idx_in_cluster]
      this_ranked_list <- sortable::add_rank_list(
        text = i,
        labels = as.list(ontologies_in_cluster),
        input_id = ns(i),
        options = sortable::sortable_options(multiDrag = TRUE)
      )
      boxes_list <- append(boxes_list, list(this_ranked_list))
    }
    do.call(what = "bucket_list", boxes_list)
  })

  observeEvent(input$move, {
    for (i in input$shown_groups[!input$shown_groups %in% "create new cluster"]){
      idx_in_cluster <- descent_data$inputData$ontoTerm %in% input[[i]]
      descent_data$inputData$clusterTerm[idx_in_cluster] <- i
    }

    if(any(input$shown_groups %in% "create new cluster")){
      idx_in_cluster <- descent_data$inputData$ontoTerm %in% input[["create new cluster"]]
      term <- relabelleR(net, GOnames, target = descent_data$inputData$ontoID[idx_in_cluster])
      descent_data$inputData$clusterTerm[idx_in_cluster] <- term

      updateCheckboxGroupInput(session = session,
                               inputId = "shown_groups",
                               choices = c(sort(unique(descent_data$inputData$clusterTerm)),
                                           "create new cluster"))
    }
  })
}

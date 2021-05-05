#' UI for the clustering page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
exploring_page_ui <- function(id)
{
  ns <- shiny::NS(id)
  fluidPage(

    titlePanel("Cluster and explore"),

    fluidRow(
     column(4,
        actionButton(ns("clusterButton"),
                     label = "Cluster!"),
        shinycssloaders::withSpinner(plotOutput(outputId = ns("netPlotOut"), height = 750,
                   brush = brushOpts(ns("netSelect"))))
        ,tableOutput(ns("test"))
      ),
     column(2,
            uiOutput(ns("shown_groups")),
            uiOutput(ns("move")))
            ,
     column(6,uiOutput(ns("sorting_boxes")))
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

#' @import shiny leiden
#' @importFrom sortable bucket_list
#' @export
exploring_page <- function(input, output, session, descent_data)
{
  ns <- session$ns
  output$netPlotOut <- NULL
  results <- NA
  y <- NA
  networkPlot <- NA

  observeEvent(input$clusterButton,{
    # putting it here so that the delay is during the clustering rather than at the firtst page
    # ------------------ App virtualenv setup (Do not edit) ------------------- #
    if (Sys.info()[['user']] == 'shiny'){
      virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
      python_path = Sys.getenv('PYTHON_PATH')

      # Create virtual env and install dependencies
      reticulate::virtualenv_create(envname = virtualenv_dir,
                                    python = python_path)
      reticulate::virtualenv_install(virtualenv_dir,
                                     packages = c('leidenalg'),
                                     ignore_installed=TRUE)
      reticulate::use_virtualenv(virtualenv_dir,
                                 required = T)
    }
    # ------------------ App server logic (Edit anything below) --------------- #

      if (length(descent_data$inputData$ontoID)==0){
        shinyWidgets::sendSweetAlert(session = session,
                                     title = "Clustering Error",
                                     text = "You have not entered any data!",
                                     type = "error")
     }
    req(descent_data$inputData)
    output$netPlotOut <- renderPlot(plot(descent_data$inputData))

    results <- clustereR(ontoNet = descent_data$net,
                         method = "leiden",
                         target = descent_data$inputData$ontoID)
    networkPlot<-results$plot




    descent_data$inputData <- merge(descent_data$inputData[,colnames(descent_data$inputData) %in%
                                                             c("ontoID",
                                                               "direction",
                                                               "pValue",
                                                               "enrichmentScore"), with = F],
                                    results$res,
                                    by = "ontoID", order = F)

    descent_data$clustered <- list(exists =  T)

    output$netPlotOut <- renderPlot({
      par(mar = c(0,0,0,0))
      set.seed(42)
      plot(networkPlot,
           layout = layout_with_drl,
             vertex.label = NA,
             vertex.label.cex = 0.5,
             vertex.border.cex = 0.000001,
             asp = 0,
             axes = F)
    })

    output$test <- renderTable({
      set.seed(42)
      V(networkPlot)$names <- names(V(networkPlot))
      y <-
        data.frame(V(networkPlot)$names, norm_coords(layout_with_drl(networkPlot)))
      res <- brushedPoints(y, input$netSelect, "X1", "X2")
      if (nrow(res) == 0)
        return()
      res
    })

      output$shown_groups <- renderUI({
        checkboxGroupInput(ns("shown_groups"),
                           label = "Select groups to show",
                           choices = "No clusters defined yet")
      })
        output$move <- renderUI ({
          actionButton(inputId = ns("move"), label = "Redefine clusters")
      })
  })
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
      term <- relabelleR(descent_data$net, target = descent_data$inputData$ontoID[idx_in_cluster])
      descent_data$inputData$clusterTerm[idx_in_cluster] <- term

      updateCheckboxGroupInput(session = session,
                               inputId = "shown_groups",
                               choices = c(sort(unique(descent_data$inputData$clusterTerm)),
                                           "create new cluster"))
    }
  })






}

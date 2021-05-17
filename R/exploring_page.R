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

    sidebarLayout(
     sidebarPanel(
        actionButton(ns("clusterButton"),
                     label = "Cluster!"),
        uiOutput(ns("shown_groups")), uiOutput(ns("move"))),
     mainPanel(shinycssloaders::withSpinner(
       plotOutput(
         outputId = ns("netPlotOut"),
         height = 750,
         brush = brushOpts(ns("netSelect")),
         hover = hoverOpts(ns("netHover"),
                           delay = 0)
       )
     )
     ,
     uiOutput(ns("sorting_boxes"))

     )))
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
  #netPlotOut is defined as NULL here to keep the spinner from appearing until
  #submit button is pressed
  output$netPlotOut <- NULL

  observeEvent(input$clusterButton, {

    # putting it here so that the delay is during the clustering rather than at the firtst page
    # ------------------ App virtualenv setup (Do not edit) ------------------- #
    if (Sys.info()[['user']] == 'shiny'){

      virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
      python_path = Sys.getenv('PYTHON_PATH')

      if(any(reticulate::virtualenv_list() == virtualenv_dir)){
        print("using existing version")
        reticulate::use_virtualenv(virtualenv_dir,
                                   required = T)
      }else{
        print("creating new version")
        reticulate::virtualenv_create(envname = virtualenv_dir,
                                      python = python_path)
        reticulate::virtualenv_install(virtualenv_dir,
                                       packages = c('leidenalg', 'python-igraph', 'numpy'),
                                       ignore_installed=TRUE)
        reticulate::use_virtualenv(virtualenv_dir,
                                   required = T)
      }

    }else{

      CONDA_NAME <- Sys.getenv("CONDA_NAME")

      if(any(reticulate::conda_list()$name == CONDA_NAME)){
        print("running old")
        reticulate::use_condaenv(CONDA_NAME, required = T)
      }else{
        reticulate::conda_create(CONDA_NAME)
        reticulate::conda_install(CONDA_NAME, packages = c("leidenalg", "python-igraph", "numpy"))
        reticulate::use_condaenv(CONDA_NAME, required = T)
      }

    }
    # ------------------ App server logic (Edit anything below) --------------- #
  #Add sweetalert for pressing Submit before reading in data
      if (length(descent_data$inputData$ontoID)==0){
        shinyWidgets::sendSweetAlert(session = session,
                                     title = "Clustering Error",
                                     text = "You have not entered any data!",
                                     type = "error")
     }
    req(descent_data$inputData)

    #add renderPlot  so the spinner will appear.
    #Otherwise it doesn't appear until clusterR is done
    output$netPlotOut <- renderPlot(plot())

    print("packages available")
    print(paste0("leiden ", reticulate::py_module_available("leidenalg")))
    print(paste0("igraph ", reticulate::py_module_available("igraph")))

    print("running ontodesc")
    results <- clustereR(ontoNet = descent_data$net,
                         method = "leiden",
                         target = descent_data$inputData$ontoID)
    print("done with ontodesc")

    networkPlot<-results$plot

    descent_data$inputData <- merge(descent_data$inputData[,colnames(descent_data$inputData) %in%
                                                             c("ontoID",
                                                               "direction",
                                                               "pValue",
                                                               "enrichmentScore"), with = F],
                                    results$res,
                                    by = "ontoID", order = F)

    ### Save Default ###
    rV$def <- reactive(results$res[,c("ontoID", "clusterNumber", "clusterTerm")])

    descent_data$clustered <- list(exists =  T)

    output$netPlotOut <- renderPlot({
      par(mar = c(0,0,0,0))
      set.seed(42)
      plot(networkPlot,
           layout = layout.auto,
             vertex.label = NA,
             vertex.label.cex = 0.5,
             vertex.border.cex = 0.000001,
             asp = 0,
             axes = F)
    })

observeEvent(input$netSelect,{
      set.seed(42)
      y <-
        data.frame(names(V(networkPlot)), norm_coords(layout.auto(networkPlot)))
      colnames(y)[1] <- "ontoID"
      y <- y %>%
        dplyr::filter(ontoID %in% results$res$ontoID)
      y <- dplyr::left_join(y, results$res, by = "ontoID") %>%
        dplyr::select(ontoTerm, X1, X2, clusterTerm)
      res <- brushedPoints(y, input$netSelect, "X1", "X2")
      if (nrow(res) == 0)
        return()
      updateCheckboxGroupInput(
        session = session,
        inputId = "shown_groups",
        selected = res$clusterTerm
      )})




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
  observeEvent(input$move,{
    shinyWidgets::sendSweetAlert(session = session,
                                 title = "Clusters have been redefined",
                                 text = "You have chosen to redefine clusters. Keep in mind that data are no longer objective and should be interpreted with caution",
                                 type = "warning")
  })


  ### Keep Default Clusters ###
  rV <- reactiveValues()

  observeEvent(input$move,{

    tempData <- reactive({
      temp <- rV$def()
      colnames(temp)[2:3] <- c("defaultClusterNumber", "defaultClusterTerm")
      return(temp)
    })

    descent_data$newOutput <- merge(descent_data$inputData, tempData(), by = "ontoID", order = FALSE)
  })

}

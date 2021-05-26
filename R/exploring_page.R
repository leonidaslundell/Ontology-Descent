#' UI for the clustering page
#'
#' @param id unique id for this module
#'
#' @import shiny
#' @export
exploring_page_ui <- function(id) {
  ns <- shiny::NS(id)
  fluidPage(
    titlePanel("Cluster and explore"),
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("clusterButton"),
                     label = "Cluster!"
        ),
        uiOutput(ns("shown_groups")),
        uiOutput(ns("move"))
      ),
      mainPanel(
        div(
          style = "position:relative",
          shinycssloaders::withSpinner(
            plotOutput(
              outputId = ns("netPlotOut"),
              height = 750,
              brush = brushOpts(ns("netSelect")),
              hover = hoverOpts(ns("netHover"),
                                delay = 100,
                                delayType = "debounce"
              )
            )
          ),
          uiOutput(ns("hover"))
        ),
        uiOutput(ns("sorting_boxes"))
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

#' @import shiny leidenAlg
#' @importFrom sortable bucket_list
#' @export
exploring_page <- function(input, output, session, descent_data) {
  ns <- session$ns

  # netPlotOut is defined as NULL here to keep the spinner from appearing until
  # submit button is pressed
  output$netPlotOut <- NULL

  observeEvent(input$clusterButton, {

    # Check if it has been clustered previously, if yes reset the sortable
    if (!exists("descent_data$clustered$exists")) {
      descent_data$clustered$exists <- F
    }

    # Add sweetalert for pressing Submit before reading in data
    if (length(descent_data$inputData$ontoID) == 0) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Clustering Error",
        text = "You have not entered any data!",
        type = "error"
      )
    }
    req(descent_data$inputData)

    # run the ontodesc
    results <- clustereR(
      ontoNet = descent_data$net,
      method = "leiden",
      target = descent_data$inputData$ontoID,
      seed = 42
    )

    # checks for whether the GOid are wrong, or whether the ontology is incorrect.
    if (!class(results) == "list") {
      results <- gsub(".*\\: ", "", results)

      if (length(results) < 5) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Input error",
          text = paste0(
            "Ontology IDs do not match network.\nMissing IDs: ",
            paste0(results, collapse = " ")
          ),
          type = "error"
        )
      } else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Input error",
          text = "Have you selected the correct ontology?",
          type = "error"
        )
      }
    }
    req(class(results) == "list")
    descent_data$clustered$exists <- T

    # merge the ontodesc with the input data
    descent_data$networkPlot <- results$plot
    descent_data$inputData <- merge(descent_data$inputData[, colnames(descent_data$inputData) %in%
                                                             c(
                                                               "ontoID",
                                                               "direction",
                                                               "pValue",
                                                               "enrichmentScore"
                                                             ), with = F],
                                    results$res,
                                    by = "ontoID", order = F
    )

    # Save Default
    rV <- reactiveValues()
    rV$def <- reactive(results$res[, c("ontoID", "clusterNumber", "clusterTerm")])

    # Keep Default Clusters when things have been moved
    observeEvent(input$move, {
      tempData <- reactive({
        temp <- rV$def()
        colnames(temp)[2:3] <- c("defaultClusterNumber", "defaultClusterTerm")
        return(temp)
      })

      descent_data$newOutput <- merge(descent_data$inputData, tempData(), by = "ontoID", order = FALSE)
    })

    output$netPlotOut <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      set.seed(42)
      plot(descent_data$networkPlot,
           # layout = norm_coords(layout_nicely(descent_data$networkPlot)),
           vertex.label = NA,
           vertex.label.cex = 0.5,
           vertex.border.cex = 0.000001,
           asp = 0,
           axes = F
      )
    })

    observeEvent(input$netSelect, {
      y <- data.frame(names(V(descent_data$networkPlot)), norm_coords(layout_nicely(descent_data$networkPlot)))
      colnames(y)[1] <- "ontoID"
      y <- y %>%
        dplyr::filter(ontoID %in% results$res$ontoID)
      y <- dplyr::left_join(y, results$res, by = "ontoID") %>%
        dplyr::select(ontoTerm, X1, X2, clusterTerm)
      res <- brushedPoints(y, input$netSelect, "X1", "X2")
      if (nrow(res) == 0) {
        return()
      }
      updateCheckboxGroupInput(
        session = session,
        inputId = "shown_groups",
        selected = res$clusterTerm
      )
    })

    output$hover <- renderUI({
      y <- data.frame(names(V(descent_data$networkPlot)), norm_coords(layout_nicely(descent_data$networkPlot)))
      colnames(y)[1] <- "ontoID"
      # y <- y %>%
      #   dplyr::filter(ontoID %in% results$res$ontoID)
      y <- dplyr::left_join(y, results$res, by = "ontoID") %>%
        dplyr::select(ontoTerm, X1, X2, clusterTerm)

      res <- nearPoints(y, input$netHover, xvar = "X1", yvar = "X2", maxpoints = 1)
      if (nrow(res)>0) {
        style <- paste0(
          "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
          "left:", res$X1 + 2, "px; top:", res$X2 + 2, "px;"
        )

        wellPanel(
          style = style,
          p(HTML(paste0(res$ontoTerm)))
        )
      }
    })

    output$shown_groups <- renderUI({
      checkboxGroupInput(ns("shown_groups"),
                         label = "Select groups to show",
                         choices = "No clusters defined yet"
      )
    })
    output$move <- renderUI({
      actionButton(inputId = ns("move"), label = "Redefine clusters")
    })
  })

  observe(req(
    descent_data$inputData$clusterNumber,
    descent_data$inputData$clusterTerm
  ))

  observeEvent(descent_data$clustered, {
    if (descent_data$clustered$exists) { # No need to edit anything if the page is not active
      updateCheckboxGroupInput(
        session = session,
        inputId = "shown_groups",
        choices = c(
          sort(unique(descent_data$inputData$clusterTerm)),
          "create new cluster"
        )
      )
    }
  })

  output$sorting_boxes <- renderUI({
    req(input$shown_groups)

    boxes_list <- list(
      header = NULL,
      group_name = "bucket_list_group",
      orientation = "horizontal"
    )

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
    for (i in input$shown_groups[!input$shown_groups %in% "create new cluster"]) {
      idx_in_cluster <- descent_data$inputData$ontoTerm %in% input[[i]]
      descent_data$inputData$clusterTerm[idx_in_cluster] <- i
    }

    if (any(input$shown_groups %in% "create new cluster")) {
      idx_in_cluster <- descent_data$inputData$ontoTerm %in% input[["create new cluster"]]

      results <- clustereR(
        ontoNet = descent_data$net,
        method = "leiden",
        target = descent_data$inputData$ontoID,
        forceCluster = descent_data$inputData$ontoID[idx_in_cluster],
        seed = 42
      )

      descent_data$inputData <- results$res

      descent_data$networkPlot <- results$plot

      updateCheckboxGroupInput(
        session = session,
        inputId = "shown_groups",
        choices = c(
          sort(unique(descent_data$inputData$clusterTerm)),
          "create new cluster"
        )
      )
    }
  })

  observeEvent(input$move, {
    # make sure to show it only once.
    if (is.null(descent_data$newOutput)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Clusters have been redefined",
        text = "You are manually redefing clusters. Please make sure to include suplemental data of your manually redefined clusters in any potential publication, and notice the changed axis label on the final plot (user defined clusters)",
        type = "warning"
      )
    }
  })
}

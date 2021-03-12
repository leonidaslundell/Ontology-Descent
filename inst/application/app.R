ui <- shinyUI(fluidPage(
  # Application title
  shiny::navbarPage("Ontology Descent: A data visualization tool for Gene Ontology Enrichment data",
                    shiny::tabPanel("Data Entry", data_entry_page_ui("data_entry")),
                    shiny::tabPanel("Clustering", clustering_page_ui("clustering")),
                    shiny::tabPanel("Sorting", sorting_page_ui("sorting")),
                    shiny::tabPanel("Plotting", plotting_page_ui("plotting")),
                    id = "active_page"
  )
))

server <- shinyServer(function(input, output, session){

  descent_data <- get_test_data()

  observe({
    descent_data$active_page <- input$active_page
  })

  shiny::callModule(data_entry_page, "data_entry", descent_data)
  shiny::callModule(clustering_page, "clustering", descent_data)
  shiny::callModule(sorting_page, "sorting", descent_data)
  shiny::callModule(plotting_page, "plotting", descent_data)
})

shiny::shinyApp(ui, server)

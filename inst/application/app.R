ui <- shinyUI(fluidPage(
  # Application title
  shiny::navbarPage(title = "Ontology Descent: summarizing and visualizing complex enrichment data",
                    shiny::tabPanel("Data Entry", data_entry_page_ui("data_entry")),
                    shiny::tabPanel("Clustering", exploring_page_ui("clustering")),
                    #shiny::tabPanel("Sorting", sorting_page_ui("sorting")),
                    shiny::tabPanel("Plotting", plotting_page_ui("plotting")),
                    id = "mainApp")
))

server <- shinyServer(function(input, output){

  descent_data <- reactiveValues()

  shiny::callModule(data_entry_page, "data_entry", descent_data)
  shiny::callModule(exploring_page, "clustering", descent_data)
  #shiny::callModule(sorting_page, "sorting", descent_data)
  shiny::callModule(plotting_page, "plotting", descent_data)
})

shiny::shinyApp(ui, server)

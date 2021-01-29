ui <- shinyUI(fluidPage(
  # Application title
  shiny::navbarPage("Ontology Descent: A data visualization tool for Gene Ontology Enrichment data",
                    shiny::tabPanel("Data Entry", data_entry_page_ui("data_entry")),
                    shiny::tabPanel("Clustering", clustering_page_ui("clustering")),
                    shiny::tabPanel("Plotting", plotting_page_ui("plotting")),
                    id = "mainApp"
  )
))

server <- shinyServer(function(input, output){
  
  descent_data <- shiny::reactiveValues(inputData = data.frame(ontoID = c("GO:0022900",
                                                                          "GO:0042773",
                                                                          "GO:0042775",
                                                                          "GO:0022904"),
                                                               ontoTerm = c("electron transport chain",
                                                                            "ATP synthesis coupled electron transport",
                                                                            "mitochondrial ATP synthesis coupled electron",
                                                                            "respiratory electron transport chain"),
                                                               enrichmentScore = c(1.3,
                                                                                   0.3,
                                                                                   -1.4,
                                                                                   3.1),
                                                               pValue = c(1.0125E-13,
                                                                          2.5000E-7,
                                                                          1.85887E-2,
                                                                          1.85887E-1),
                                                               direction = factor(c("Up", "Up", "Down", "Up"), 
                                                                                  levels = c("Up", "Down"))
                                                               ),
                                        clusterData = data.frame(ontoID = c("GO:0022900",
                                                                            "GO:0042773",
                                                                            "GO:0042775",
                                                                            "GO:0022904"),
                                                                 ontoTerm = c("electron transport chain",
                                                                              "ATP synthesis coupled electron transport",
                                                                              "mitochondrial ATP synthesis coupled electron",
                                                                              "respiratory electron transport chain"),
                                                                 enrichmentScore = c(1.3,
                                                                                     0.3,
                                                                                     -1.4,
                                                                                     3.1),
                                                                 pValue = c(1.0125E-13,
                                                                            2.5000E-7,
                                                                            1.85887E-2,
                                                                            1.85887E-1),
                                                                 direction = factor(c("Up", "Up", "Down", "Up"), 
                                                                                    levels = c("Up", "Down")),
                                                                 clusterNumber = c(1,1,2,3),
                                                                 clusterName = c("Energy","Energy","Muscle","Bone")
                                                                 )
                                        )
                                        
                                        shiny::callModule(data_entry_page, "data_entry", descent_data)
  shiny::callModule(clustering_page, "clustering", descent_data)
  shiny::callModule(plotting_page, "plotting", descent_data)
})

shiny::shinyApp(ui, server)

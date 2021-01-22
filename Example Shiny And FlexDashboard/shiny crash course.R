library(shiny)
library(DT)
# whatever
dat <- data.frame(ensmbl = paste0("ENSMUSG", seq(10000,
                                                 19999,
                                                 1)[sample(9999, 
                                                           1000, 
                                                           replace = F)]),
                  FC = rnorm(1000,0,4),
                  pVal = seq(0,1,0.000001)[sample(1000001,1000,replace = T)])

dat[dat$pVal<0.05,]

rm(list=c("ui", "server"))

ui <- fluidPage(
    textInput("gene", "Type the ensmb gene", value = "ENSMUSG13286"),
    actionButton("press", "plot"),
    plotOutput("plot"),
    dataTableOutput("table"),
    verbatimTextOutput("history")
    )

server <- function(input, output, session) {
  
  observeEvent(input$press, {
    
    x <- dat[dat$ensmbl == input$gene,]
    
    output$plot <- renderPlot(plot(x[,"FC"], 
                                   ylim = c(-12,12)))
    
  })
  
  history <- reactiveValues(x = NULL)
  observeEvent(input$press, {
    
    history$x <- c(history$x, input$gene)
    
  })
  output$table <- renderDataTable(dat)
  
  output$history <- renderText(history$x)
}

shinyApp(ui, server)

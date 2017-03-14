library(shiny)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$cloud <- renderPlot({ 
    if(input$keyword!="")
    {
      getcloud(input$keyword)
      
    } 
    
  })
  
  
})#main
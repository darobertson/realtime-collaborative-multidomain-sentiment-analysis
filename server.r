library(shiny)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$text1 <- renderImage({ 
    if(input$keyword!="")
    {
      getvote(input$keyword)
      
      
      
    }
    else
    {
      filename <- normalizePath(file.path('./www',
                                          paste('collab', '.jpeg', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename)
    }
    
  }, deleteFile = FALSE)
  
  
  output$word <- renderPlot({ 
    if(input$keyword!="")
    {
    getword(input$keyword)
    }
  })
  
})#main
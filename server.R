library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define a server for the Shiny app
shinyServer(function(input, output) {
 
  #man -> make, cyl -> dealer, trans -> model
  # Filter data based on selections
  output$table <- renderDataTable({
    #data <- df
    load('ndf.RData')
    if (input$man != "All"){
      df <- df[df$Make == input$man,]
    }
    if (input$dealer != "All"){
      df <- df[df$Dealership == input$dealer,]
    }
    if (input$model != "All"){
      df <- df[df$Model == input$model,]
    }
    df
  })
  
})


library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  #man -> make, cyl -> dealer, trans -> model
  # Filter data based on selections
  output$table <- renderDataTable({
    data <- chev_df
    if (input$man != "All"){
      data <- data[data$Make == input$man,]
    }
    if (input$dealer != "All"){
      data <- data[data$Dealership == input$dealer,]
    }
    if (input$model != "All"){
      data <- data[data$Model == input$model,]
    }
    data
  })
  
})


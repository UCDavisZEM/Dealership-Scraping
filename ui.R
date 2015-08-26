library(shiny)

# Load the ggplot2 package which provides
# the 'df' dataset.
library(ggplot2)
load('madata.rdata')
df <- madata
# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Vehicle Local Dealerships"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
             selectInput("man", 
                         "Manufacturer:", 
                         c("All", 
                           unique(as.character(df$Make))))
      ),
      column(4, 
             selectInput("dealer", 
                         "Dealership:", 
                         c("All", 
                           unique(as.character(df$Dealership))))
             selectInput()
      ),       
      column(4, 
             selectInput("model", 
                         "Model:", 
                         c("All", 
                           unique(as.character(df$Model))))
  
      )       
    ),
    # Create a new row for the table. 
    fluidRow(
      dataTableOutput(outputId="table")
    )    
  )  
)

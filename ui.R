library(shiny)

# Load the ggplot2 package which provides
# the 'chev_df' dataset.
library(ggplot2)

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
                           unique(as.character(chev_df$manufacturer))))
      ),
      column(4, 
             selectInput("dealer", 
                         "Dealership:", 
                         c("All", 
                           unique(as.character(chev_df$Dealership))))
      ),       
      column(4, 
             selectInput("model", 
                         "Model:", 
                         c("All", 
                           unique(as.character(chev_df$Model))))
      )       
    ),
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId="table")
    )    
  )  
)

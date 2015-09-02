library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)

header <- dashboardHeader(
  title = "Date Visualization for Local Dealerships Project",
  titleWidth = 450
)

sider <- dashboardSidebar(width = 175,
  sidebarMenu(
    menuItem("Data Table",tabName = "datatable",icon = icon("database")),
    menuItem("Charts",tabName = "charts", icon = icon("bar-chart")),
    menuItem("Interactive Map", tabName="map", icon = icon("map-marker"))
   )
  )

body <- dashboardBody(
   fluidPage(
     tabItems(
       tabItem("datatable",
               fluidRow(
                 valueBoxOutput("newInventoryBox"),
                 valueBoxOutput("countOfEVsBox"),
                 valueBoxOutput("countOfDealersBox")
                 ),
               fluidRow(
                 uiOutput("boxMan"),
                 uiOutput("boxDealer"),       
                 uiOutput("boxModel") 
                ),
               fluidRow(
                 dataTableOutput(outputId="table")
                )
               ),
        tabItem("charts",
                fluidRow( box(title="Bar plot",status="primary", solidHeader=TRUE, plotlyOutput("EVBarplot"))),
                fluidRow( box(title="Bubble plot",status="primary", solidHeader = TRUE, htmlOutput("EVDLbycounty"),width=850,height=725))
                ),
        tabItem("map", #   fluidRow()
                div(class="outer",
                    
                    tags$head(
                      # Include our custom CSS
                      includeCSS("styles.css")
                      #includeScript("gomap.js")
                    ),
                    
                    leafletOutput("map", width="100%", height="100%")
                    
                    )
                )
       )
     )
   )

ui <- dashboardPage(header, sider, body, skin ='blue')
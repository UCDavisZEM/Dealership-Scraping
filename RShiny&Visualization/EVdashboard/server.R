library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(googleVis)
library(plotly)
library(leaflet)

load('finalMA.rdata')
df <- madata %>%
      select(Dealership,Make,VIN,Model,Trim,isEV,county_name)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$boxMan = renderUI(column(4, selectInput("man","Manufacturer",c("All", 
                                                                        unique(as.character(df$Make))), "All")))
  output$boxDealer = renderUI(column(4,
                                     if(input$man=='All') {
                                       selectInput("dealer", 
                                                   "Dealership:", 
                                                   c("All", 
                                                     unique(as.character(df$Dealership))))
                                     }
                                     else selectInput("dealer", "Dealership",c("All", 
                                                                               unique(as.character(df$Dealership[which(df$Make == input$man)]))), "All"))
  )
  output$boxModel = renderUI(column(4,
                                    if(input$man =='All' && input$dealer=='All') {
                                      selectInput("model", 
                                                  "Model:", 
                                                  c("All", 
                                                    unique(as.character(df$Model)))) 
                                    }else if(input$man =='All' && input$dealer!='All') {
                                      selectInput("model", 
                                                  "Model:", 
                                                  c("All", 
                                                    unique(as.character(df$Model[which(df$Dealership == input$dealer)])))) 
                                    }
                                    else selectInput('model', "Model", c("All", 
                                                                         unique(as.character(df$Model[which(df$Make == input$man)]))), "All"))
  )
  
  subdata0 = reactive(df)
  subdata1 = reactive(subdata0()[which(subdata0()$Make == input$man),])
  subdata2 = reactive(subdata1()[which(subdata1()$Dealership == input$dealer),])
  subdata3 = reactive(subdata2()[which(subdata2()$Model == input$model),])
  
  dealerdata2 = reactive(subdata0()[which(subdata0()$Dealership == input$dealer),])
  modeldata3 = reactive(subdata0()[which(subdata0()$Model == input$model),])
  dealermodeldata = reactive(subdata0()[which(subdata0()$Dealership == input$dealer && subdata0()$Model == input$model),])
  
  output$table <- renderDataTable({
    if(input$man == "All" && input$dealer == 'All' && input$model == 'All') {return(subdata0())
    }
    if (input$man != 'All' && input$dealer == 'All' && input$model == 'All') {return(subdata1())
    }
    if (input$man != 'All' && input$dealer != 'All' && input$model == 'All') {return(subdata2())
    } 
    if (input$man != 'All' && input$dealer != 'All' && input$model != 'All') {return(subdata3())
    } 
    if (input$man != 'All' && input$dealer == 'All' && input$model != 'All') {return(modeldata3())     
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model == 'All') {return(dealerdata2())
    }
    if (input$man == 'All' && input$dealer == 'All' && input$model != 'All') {return(modeldata3())
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model != 'All') {return(dealermodeldata())
    }                                                                         
  })
  
  #Display values of simple statistics about vehicles
  output$newInventoryBox <- renderValueBox({
    if(input$man == "All" && input$dealer == 'All' && input$model == 'All') {
       cnt <- as.numeric(subdata0() %>% summarise(n_distinct(VIN)))
    }
    if (input$man != 'All' && input$dealer == 'All' && input$model == 'All') {
       cnt <- as.numeric(subdata1() %>% summarise(n_distinct(VIN)))
    }
    if (input$man != 'All' && input$dealer != 'All' && input$model == 'All') {
      cnt <- as.numeric(subdata2() %>% summarise(n_distinct(VIN)))
    } 
    if (input$man != 'All' && input$dealer != 'All' && input$model != 'All') {
      cnt <- as.numeric(subdata3() %>% summarise(n_distinct(VIN)))
    } 
    if (input$man != 'All' && input$dealer == 'All' && input$model != 'All') {
      cnt <- as.numeric(modeldata3() %>% summarise(n_distinct(VIN)))
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model == 'All') {
      cnt <- as.numeric(dealerdata2() %>% summarise(n_distinct(VIN)))
    }
    if (input$man == 'All' && input$dealer == 'All' && input$model != 'All') {
      cnt <- as.numeric(modeldata3() %>% summarise(n_distinct(VIN)))
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model != 'All') {
      cnt <- as.numeric(dealermodeldata() %>% summarise(n_distinct(VIN)))
    }
    
    valueBox(
      value = format(cnt,format='d',big.mark = ',')
        , "New Inventory", icon = icon("shopping-cart"),color="blue")
  })
  
  output$countOfEVsBox <- renderValueBox({
    if(input$man == "All" && input$dealer == 'All' && input$model == 'All') {
      cnt <- as.numeric(subdata0() %>% summarise(sum(isEV)))
    }
    if (input$man != 'All' && input$dealer == 'All' && input$model == 'All') {
      cnt <- as.numeric(subdata1() %>% summarise(sum(isEV)))
    }
    if (input$man != 'All' && input$dealer != 'All' && input$model == 'All') {
      cnt <- as.numeric(subdata2() %>% summarise(sum(isEV)))
    } 
    if (input$man != 'All' && input$dealer != 'All' && input$model != 'All') {
      cnt <- as.numeric(subdata3() %>% summarise(sum(isEV)))
    } 
    if (input$man != 'All' && input$dealer == 'All' && input$model != 'All') {
      cnt <- as.numeric(modeldata3() %>% summarise(sum(isEV)))
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model == 'All') {
      cnt <- as.numeric(dealerdata2() %>% summarise(sum(isEV)))
    }
    if (input$man == 'All' && input$dealer == 'All' && input$model != 'All') {
      cnt <- as.numeric(modeldata3() %>% summarise(sum(isEV)))
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model != 'All') {
      cnt <- as.numeric(dealermodeldata() %>% summarise(sum(isEV)))
    }
    
    valueBox(
      value = format(cnt, format='d', big.mark = ',')
        , "New EVs", icon = icon("car"),color="blue")
  })
  
  output$countOfDealersBox <- renderValueBox({
    if(input$man == "All" && input$dealer == 'All' && input$model == 'All') {
      cnt <- as.numeric(subdata0() %>% summarise(n_distinct(Dealership)))
    }
    if (input$man != 'All' && input$dealer == 'All' && input$model == 'All') {
      cnt <- as.numeric(subdata1()  %>% summarise(n_distinct(Dealership)))
    }
    if (input$man != 'All' && input$dealer != 'All' && input$model == 'All') {
      cnt <- as.numeric(subdata2() %>% summarise(n_distinct(Dealership)))
    } 
    if (input$man != 'All' && input$dealer != 'All' && input$model != 'All') {
      cnt <- as.numeric(subdata3() %>% summarise(n_distinct(Dealership)))
    } 
    if (input$man != 'All' && input$dealer == 'All' && input$model != 'All') {
      cnt <- as.numeric(modeldata3() %>% summarise(n_distinct(Dealership))) 
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model == 'All') {
      cnt <- as.numeric(dealerdata2() %>% summarise(n_distinct(Dealership)))
    }
    if (input$man == 'All' && input$dealer == 'All' && input$model != 'All') {
      cnt <- as.numeric(modeldata3() %>% summarise(n_distinct(Dealership)))
    }
    if (input$man == 'All' && input$dealer != 'All' && input$model != 'All') {
      cnt <- as.numeric(dealermodeldata() %>% summarise(n_distinct(Dealership)))
    }
    
    valueBox(
      value = format(cnt,format='d', big.mark = ',')
        , "Dealerships", icon = icon("map-marker"),color="blue")
  })
  
  output$EVDLbycounty <- renderGvis({
    EVsbyCounty_df <- madata %>% group_by(county_name, Make)
    EVDLsbyCounty_df <- EVsbyCounty_df  %>% summarise(CountOfVeh = n(), 
                                                      CountOfEVs = sum(isEV), 
                                                      CountOfDealers = n_distinct(Dealership)) %>% 
      select(county_name, Make,CountOfVeh,CountOfEVs,CountOfDealers)
    
    EV_Bubble <- gvisBubbleChart(EVDLsbyCounty_df, idvar = "county_name", xvar = 'CountOfEVs', yvar = 'CountOfVeh', colorvar = 'Make',
                                 sizevar = 'CountOfDealers',options=list(
                                   explorer="{actions: ['dragToZoom', 
                                                                              'rightClickToReset'],
                                                                               maxZoomIn:0.05}",
                                   vAxis="{title:'New Inventory',minValue:-5}",
                                   hAxis="{title:'Number of EVs',textPosition:'out',minValue:-10}",
                                   title="Correlations between numbers of electric vehiclels, new inventory, local dealerships and Manufacturers in MA",
                                   width=850, height=650, bubble.opacity=0.5,
                                   bubble = "{textStyle:{color:'none'}}"))
    
    EV_Bubble
  })
  
  output$EVBarplot <- renderPlotly({
    bar_df <- madata %>%
      select(Make,VIN,isEV) %>%
      group_by(Make) %>%
      summarise(CountOfVeh = n_distinct(VIN), CountOfEV = sum(isEV))
    
    bar_df %>%
      plot_ly(x = Make, y = CountOfVeh, name = "New Vehicles", type = "bar",filename='newInventory-bar') %>%
      add_trace(x = Make, y = CountOfEV, name = "New EVs") %>%
      layout(xaxis = list(title="Make"), yaxis = list(title="Count of New Vehicles"), barmode = "stack", 
             title='New Inventories for Manufacturers')
  })
  
  output$map <- renderLeaflet({
    test <- madata %>%
              select(Dealership, Address, GeoLatitude, GeoLongitude) %>%
              distinct()
    
    leaflet(test[1:50,],padding = c(0,0,0,100)) %>%
      addTiles() %>%
      addMarkers(lng = ~GeoLongitude, lat = ~GeoLatitude)
  })
  
})
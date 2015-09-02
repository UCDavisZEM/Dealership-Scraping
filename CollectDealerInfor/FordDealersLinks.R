plotly:::verify("username")
plotly:::verify("api_key")

Sys.setenv("plotly_username"="mrapple")
Sys.setenv("plotly_api_key"="bhy44rb4es")

library("plotly")

bar_df = madata %>%
  select(Make,VIN,isEV) %>%
  group_by(Make) %>%
  summarise(CountOfVeh = n_distinct(VIN), CountOfEV = sum(isEV))

bar_df %>%
  plot_ly(x = Make, y = CountOfVeh, name = "New Vehicles", type = "bar",filename='newInventory-bar') %>%
  add_trace(x = Make, y = CountOfEV, name = "New EVs") %>%
  layout(xaxis = list(title="Make"), yaxis = list(title="Count of New Vehicles"), barmode = "stack", 
         title='New Inventories for Manufacturers')

library(leaflet)
leaflet() %>% addTiles() %>% addControl(
  position = 'bottomright'
)
  
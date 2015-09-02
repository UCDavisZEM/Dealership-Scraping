#Experiment some plots/visualization to include into RShiny Dashboard

library(dplyr)
library(googleVis)

load("finalMA.rdata")
names(madata)
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
                                                                    width=1000, height=750, bubble.opacity=0.6,
                                                                    bubble = "{textStyle:{color:'none'}}"))

plot(EV_Bubble)

data.frame(EVDLsbyCounty_df)

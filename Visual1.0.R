###percentage by make
load("finalMA.rdata")
library(ggplot2)


testdata = madata
tt = group_by(testdata, county_name)
aa = summarise(tt, numCar = n(), zipcode = unique(zipcode)[1], numEV = sum(isEV), numPEV = sum(Type %in% c("PHEV", "BEV")),numPHEV = sum(Type %in% c("PHEV")), numBEV = sum(Type %in% c("BEV")),  Longitude = mean(GeoLongitude), Latitude = mean(GeoLatitude), state = unique(state), 
               numFord = sum(Make=="FORD"), numNissan = sum(Make=="NISSAN"), numSmart = sum(Make=="SMART"), numToyota = sum(Make=="TOYOTA"), numHonda = sum(Make=="HONDA"), numChevy = sum(Make=="CHEVROLET"))
#bb = summarise(tt,numLeaf = n(Type=="BEV" & Model=="LEAF"))           
#numLeaf = sum(filter(Type, Type=="BEV", Model=="LEAF"))
 #              numFit = sum(Type=="BEV" & Model=="FIT EV"), 
  #             numSMART = sum(Type=="BEV" & Model=="FORTWO"), 
   #            numFOCUSEV = sum(Type == "BEV" & Model=="FOCUS"), 
    #           numFusion =sum(Type == "PHEV" & Model=="FUSION"), 
     #          numCMAX = sum(Type=="PHEV" &Model=="C-MAX"), 
      #         numVOLT = sum(Type=="PHEV" & Model == "VOLT"), 
       #        numPRIUS = sum(testdata$Type[testdata$Type=="PHEV" & testdata$Model =="PRIUS HYBRID"]))

m1 = leaflet(aa) %>% 
  addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
           attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
             radius = ~sqrt(numCar) * 100, popup = ~paste(sep = "<br/>", paste0("<strong>", county_name,"</strong>"),  paste0("PEV: ", numPEV), paste0("Car: ", numCar))
  ) 
library(htmlwidgets)
saveWidget(m1, file="map1.html")


names(testdata)[11:12] = c("latitude", "longitude")


m2 = leaflet(testdata) %>% 
  addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
  addMarkers(clusterOptions = markerClusterOptions() )
saveWidget(m2, file="map2.html")
###
#Venn
grid.newpage()
draw.triple.venn(area1 = 2040, area2 = 192, area3 = 2, n12 = 0, n23 = 0, n13 = 0, 
                 n123 = 0, category = c("Cars", "Electrical Cars", "Plug-in EV"), lty = "blank", 
                 fill = c("skyblue", "pink1", "mediumorchid"))

#pie chart for
data = as.data.frame(aa[1, c(6,7)])
dat = as.data.frame(t(data.frame(BEV = data$numBEV, PHEV = data$numPHEV)))
names(dat) = "Count"
dat$name = row.names(dat)
dat$name = paste0(dat$name, "-",round(dat$Count/sum(dat$Count)*100, digits =2), "%")
p <- ggplot(dat, aes(x=1, y=Count, fill=name)) +
  ggtitle("Types of PEV") +
  # black border around pie slices
  geom_bar(stat="identity", color='white') +
  # remove black diagonal line from legend
  guides(fill=guide_legend(override.aes=list(colour=NA))) +
  coord_polar(theta='y') +
  # label aesthetics
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank(),
        axis.text.x=element_text(color='black')) +
  # pie slice labels
  scale_y_continuous(
    breaks=cumsum(dat$Count)-dat$Count/2,
    labels=dat$name)+ 
 
  scale_fill_brewer(palette="BuPu", name="Category",
                    labels=dat$name)
print(p)

#####
ggplot(data=data, aes(x=lable, y=Number, fill=Number)) +
  geom_bar(fill="grey", width=.8, stat="identity")  +
  geom_text(aes(label=Number), vjust=0)+
  guides(fill=FALSE)+
  xlab("Make")+
  ggtitle("Number of Cars by Make")+
  coord_flip()

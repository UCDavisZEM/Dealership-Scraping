require(xlsx)
#setwd("~/Documents/GRADLIFE/summer/GSR-web")

#a = read.xlsx("vinpattern.xlsx", 1)
#VinPattern = a[,1:7]
#VinPattern$make = toupper(VinPattern$make)
#VinPattern$model = toupper(VinPattern$model)
#VinPattern$trim = toupper(VinPattern$trim)
#VinPattern$EV = as.character(VinPattern$EV)
#fordvin = VinPattern[VinPattern$make=='FORD',]
#chevroletvin = VinPattern[VinPattern$make=='CHEVROLET',]
#chev.p = chevroletvin[chevroletvin$location == "4,5",]
#chev.t = chevroletvin[chevroletvin$location == "5,6",]
#toyotavin = VinPattern[VinPattern$make=='TOYOTA',]
#hondavin = VinPattern[VinPattern$make=='HONDA',]
#nissanvin = VinPattern[VinPattern$make=='NISSAN',]
#smartvin = VinPattern[VinPattern$make=='SMART',]
#yearvin = data.frame(c("A","B","C","D","E","F","G"))
#names(yearvin) = "vincode"
#yearvin$year = 2010:2016

#save(VinPattern, fordvin, chevroletvin, chev.p, chev.t, toyotavin, hondavin, nissanvin, smartvin, yearvin, file = "vindecoder.rda")

load("vindecoder.rda")

##FORD MATCH
#make = "FORD"
#VIN = "1FT7X2BT1BEA66186"
getinfo.ford = function(VIN){
  vin.year = substr(VIN, 10, 10)
  year = yearvin$year[which(yearvin$vincode == vin.year)]
  info.vin = substr(VIN, 5,7)
  index = fordvin$vin == info.vin
  model = fordvin$model[index][1]
  trim = fordvin$trim[index][1]
  type = fordvin$EV[index][1]
  return(c(Year = year, Model = model, Trim = trim, Type = type))
}
#getinfo.ford(VIN)


##HONDA MATCH
#make = "HONDA"
#VIN = "1HGCR2F82DA155484"
getinfo.honda = function(VIN){
  vin.year = substr(VIN, 10, 10)
  year = yearvin$year[which(yearvin$vincode == vin.year)]
  info.vin = substr(VIN, 4,8)
  index = hondavin$vin == info.vin
  model = hondavin$model[index][1]
  trim = hondavin$trim[index][1]
  type = hondavin$EV[index][1]
  return(c(Year = year, Model = model, Trim = trim, Type = type))
  
}

##NISSAN MATCH
#make = "NISSAN"
#VIN = "JN8AS5MV1CW404903"
#VIN = "5N1AA0NE0FN609280"
getinfo.nissan = function(VIN){
  vin.year = substr(VIN, 10, 10)
  year = yearvin$year[which(yearvin$vincode == vin.year)]
  info.vin = substr(VIN,5,6)
  index = nissanvin$vin == info.vin
  model = nissanvin$model[index][1]
  trim = nissanvin$trim[index][1]
  type = nissanvin$EV[index][1]
  return(c(Year = year, Model = model, Trim = trim, Type = type))
}
#getinfo.nissan(VIN)

##TOYOTA MATCH
#make = "TOYOTA"
#VIN = "4T4BF1FK3FR448855"
#VIN = "5TDYK3DC8FS634133"
#VIN = "JTDKN3DU0F0432010"
getinfo.toyota = function(VIN){
  vin.year = substr(VIN, 10, 10)
  year = yearvin$year[which(yearvin$vincode == vin.year)]
  info.vin = substr(VIN,4,8)
  index = toyotavin$vin == info.vin
  model = toyotavin$model[index][1]
  trim = toyotavin$trim[index][1]
  type = toyotavin$EV[index][1]
  return(c(Year = year, Model = model, Trim = trim, Type = type))
}
#getinfo.toyota(VIN)

##CHEVROLET
#make = "CHEVROLET"
#VIN = "KL7CJKSB2FB125726"
getinfo.chevrolet = function(VIN){
  vin.year = substr(VIN, 10, 10)
  year = yearvin$year[which(yearvin$vincode == vin.year)]
  info.vin = substr(VIN,4,6)

  if(substr(info.vin,3,3) %in% c(1:6)){
    info.vin.p = substr(VIN,4,5)
    index = chev.p$vin == info.vin.p
    model = chev.p$model[index][1]
    trim = chev.p$trim[index][1]
    type = chev.p$EV[index][1]
    
    if(is.na(model)== TRUE){
      info.vin.t = substr(VIN,5,6)
      index = chev.t$vin == info.vin.t
      model = chev.t$model[index][1]
      trim = chev.t$trim[index][1]
      type = chev.t$EV[index][1]
      
    }
  }
  else{
    info.vin.t = substr(VIN,5,6)
    index = chev.t$vin == info.vin.t
    model = chev.t$model[index][1]
    trim = chev.t$trim[index][1]
    type = chev.t$EV[index][1]
  }
  
  return(c(Year = year, Model = model, Trim = trim, Type = type))
}
getinfo.chevrolet(VIN)

####SMART MATCH
VIN = "WMEEJ9AA4FK840778"
getinfo.smart = function(VIN){
  vin.year = substr(VIN, 10, 10)
  year = yearvin$year[which(yearvin$vincode == vin.year)]
  info.vin = substr(VIN,4,7)
  index = smartvin$vin == info.vin
  model = smartvin$model[index][1]
  trim = smartvin$trim[index][1]
  type = smartvin$EV[index][1]
  return(c(Year = year, Model = model, Trim = trim, Type = type))
  
}
#getinfo.smart(VIN)

######Main 

getinfo = function(make, VIN){
  print(VIN)
  if(is.na(make)){
    year = NA
    trim = "NA"
    model = "NA"
    type = "NA"
  }
  else if(make=="CHEVROLET"){
    year = as.numeric(getinfo.chevrolet(VIN)["Year"])
    trim = getinfo.chevrolet(VIN)["Trim"]
    model = getinfo.chevrolet(VIN)["Model"]
    type = getinfo.chevrolet(VIN)["Type"]
  }
  else if(make=="FORD"){
    year = as.numeric(getinfo.ford(VIN)["Year"])
    trim = getinfo.ford(VIN)["Trim"]
    model = getinfo.ford(VIN)["Model"]
    type = getinfo.ford(VIN)["Type"]
  }
  else if(make=="NISSAN"){
    year = as.numeric(getinfo.nissan(VIN)["Year"])
    trim = getinfo.nissan(VIN)["Trim"]
    model = getinfo.nissan(VIN)["Model"]
    type = getinfo.nissan(VIN)["Type"]
  }
  else if(make=="HONDA"){
    year = as.numeric(getinfo.honda(VIN)["Year"])
    trim = getinfo.honda(VIN)["Trim"]
    model = getinfo.honda(VIN)["Model"]
    type = getinfo.honda(VIN)["Type"]
  }
  else if(make=="TOYOTA"){
    year = as.numeric(getinfo.toyota(VIN)["Year"])
    trim = getinfo.toyota(VIN)["Trim"]
    model = getinfo.toyota(VIN)["Model"]
    type = getinfo.toyota(VIN)["Type"]
  } 
  else if(make == "SMART"){
    year = as.numeric(getinfo.smart(VIN)["Year"])
    trim = getinfo.smart(VIN)["Trim"]
    model = getinfo.smart(VIN)["Model"] 
    type = getinfo.smart(VIN)["Type"]
  }
  else if(!make %in% c("SMART", "TOYOTA", "HONDA", "NISSAN", "FORD","CHEVROLET")){
    year = NA
    model = "NA"
    trim = "NA"
    type = "NA"
  }

  return(c(Year = year, Model = model, Trim = trim, Type = type, VIN = VIN))
}

test.decoder = function(df){
  alldata = mapply(getinfo, make=df$Make, VIN = df$VIN)
  
  tt=t(as.data.frame(alldata, row.names=NULL,stringsAsFactors = F))
  rownames(tt) = NULL
  tt = as.data.frame(tt, stringsAsFactors = F)
  return(tt)
}

load("ndf.rdata")
df$Make = toupper(df$Make)
df$VIN = gsub("^\\s+|\\s+$", "",df$VIN)
testdecoder = test.decoder(df)
names(testdecoder) = c("Year", "Model", "Trim", "Type", "VIN")
madata = df
madata$Year = testdecoder$Year
madata$Trim = testdecoder$Trim
madata$Model  = testdecoder$Model
madata$Type = testdecoder$Type 
rownames(df) = NULL
index = is.na(madata$Year)
madata = madata[!index, ]
madata$Type = gsub(" ", NA,madata$Type)
madata$isEV = !is.na(madata$Type)
save(madata, file = "new-madata.rdata")

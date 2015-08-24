require(xlsx)
#setwd("~/Documents/GRADLIFE/summer/GSR-web")

#a = read.xlsx("vinpattern.xlsx", 1)
#VinPattern = a[,1:6]
#VinPattern$make = toupper(VinPattern$make)
#VinPattern$model = toupper(VinPattern$model)
#VinPattern$trim = toupper(VinPattern$trim)
#fordvin = VinPattern[VinPattern$make=='FORD',]
#chevroletvin = VinPattern[VinPattern$make=='CHEVROLET',]
#chev.p = chevroletvin[chevroletvin$location == "4,5",]
#chev.t = chevroletvin[chevroletvin$location == "5,6",]
#toyotavin = VinPattern[VinPattern$make=='TOYOTA',]
#hondavin = VinPattern[VinPattern$make=='HONDA',]
#nissanvin = VinPattern[VinPattern$make=='NISSAN',]
#yearvin = data.frame(c("A","B","C","D","E","F","G"))
#names(yearvin) = "vincode"
#yearvin$year = 2010:2016

#save(VinPattern, fordvin, chevroletvin, chev.p, chev.t, toyotavin, hondavin, nissanvin, yearvin, file = "vindecoder.rda")

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
  return(c(year = year, model = model, trim = trim))
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
  return(c(year = year, model = model, trim = trim))
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
  return(c(year = year, model = model, trim = trim))
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
  return(c(year = year, model = model, trim = trim))
}
#getinfo.toyota(VIN)

##CHEVROLET
#make = "CHEVROLET"
#VIN = "3N63M0YN0FK709902"
getinfo.chevronlet = function(VIN){
  vin.year = substr(VIN, 10, 10)
  year = yearvin$year[which(yearvin$vincode == vin.year)]
  info.vin = substr(VIN,4,6)

  if(substr(info.vin,3,3) %in% c(1:6)){
    info.vin.p = substr(VIN,4,5)
    index = chev.p$vin == info.vin.p
    model = chev.p$model[index][1]
    trim = chev.p$trim[index][1]
    if(is.na(model)== TRUE){
      info.vin.t = substr(VIN,5,6)
      index = chev.t$vin == info.vin.t
      model = chev.t$model[index][1]
      trim = chev.t$trim[index][1]
    }
  }
  else{
    info.vin.t = substr(VIN,5,6)
    index = chev.t$vin == info.vin.t
    model = chev.t$model[index][1]
    trim = chev.t$trim[index][1]
  }
  return(c(year = year, model = model, trim = trim))
}




######Main 

getinfo = function(make, VIN){
  print(VIN)
  if(is.na(make)){
    year = NA
    trim = "NA"
    model = "NA"
  }
  else if(make=="CHEVROLET"){
    year = as.numeric(getinfo.chevronlet(VIN)["year"])
    trim = getinfo.chevronlet(VIN)["trim"]
    model = getinfo.chevronlet(VIN)["model"]
  }
  else if(make=="FORD"){
    year = as.numeric(getinfo.ford(VIN)["year"])
    trim = getinfo.ford(VIN)["trim"]
    model = getinfo.ford(VIN)["model"]
  }
  else if(make=="NISSAN"){
    year = as.numeric(getinfo.nissan(VIN)["year"])
    trim = getinfo.nissan(VIN)["trim"]
    model = getinfo.nissan(VIN)["model"]
  }
  else if(make=="HONDA"){
    year = as.numeric(getinfo.honda(VIN)["year"])
    trim = getinfo.honda(VIN)["trim"]
    model = getinfo.honda(VIN)["model"]
  }
  else if(make=="TOYOTA"){
    year = as.numeric(getinfo.toyota(VIN)["year"])
    trim = getinfo.toyota(VIN)["trim"]
    model = getinfo.toyota(VIN)["model"]
  } 
  return(c(year = year, model = model, trim = trim, VIN = VIN))
}

test.decoder = function(df){
  alldata = mapply(getinfo, make=df$Make, VIN = df$VIN)
  
  tt=t(as.data.frame(alldata, row.names=NULL,stringsAsFactors = F))
  rownames(tt) = NULL
  tt = as.data.frame(tt, stringsAsFactors = F)
  return(tt)
}

load("df.rdata")
df$Make = toupper(df$Make)
testdecoder = test.decoder(df)

df$Year = testdecoder$year
df$Trim = testdecoder$trim
df$Model  = testdecoder$model
rownames(df) = NULL
save(df, file = "newdf.rdata")

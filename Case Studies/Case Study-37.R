#special BMW
#url = "http://www.bmwofalexandria.com/new_bmw.cfm"


alldata.37 = function(url){
  nurl = "http://www.bmwofalexandria.com/inventory.cfm?inventoryID=&listType=C&NewUsed=1&specialFlag=&Year=&MakeID=4&ModelID=&VehicleTypeID=&Trim=&TransmissionID=&drive_train=&priceRange=&Mileage=&exteriorColor=&interiorColor=&usedHistory=&fuelType=&CityMPG=&HywMPG=&specialOffers=0&special_offer_id=&sortBy=5&orderBy=0&vehicleNumType=S&vehicleNumVal=&displayNum=35&currPage=1&currSection=1&startPage=1&displayPage=7&showAll=1&sortOrderBy=5%2C0&sortOrderBy=5%2C0"
  doc = htmlParse(nurl)
  vin.node = getNodeSet(doc, "//tr[contains(., 'VIN')]")
  
  vins = unique(gsub(".*([0-9A-z]{17}).*","\\1",xmlApply(vin.node, xmlValue)))
  vins =  vins[nchar(vins)==17]
  make = "BMW"
  model = "NA"
  trim = "NA"
  year = NA
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
  
}

#cc = alldata.37(url)

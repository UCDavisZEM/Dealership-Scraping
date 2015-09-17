##Case study 25
#pattern new-cars.aspx/bmw.aspx
library(RCurl)
#url = "http://www.hondaofdanbury.com/new-cars.aspx"
#url = "http://www.bmwnorthscottsdale.com/new-cars.aspx"
#url = "http://www.bmwofontario.com/bmw.aspx"
#url = "http://www.bmwofsandiego.com/bmw.aspx"
getdatacontent.25 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getLinklist.25 = function(url){
  baselink = url
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  totalpage = max(as.numeric(unique(xpathSApply(doc, "//div[@class='PageBox']", xmlValue))))
  Linklist = paste0(baselink, "?_page=", 1:totalpage)
  return(Linklist)
}

scrapeInfo.25 <- function(url)
{
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  vin.node = getNodeSet(doc, "//div[contains(.,'VIN #:')]")
  vins = unique(gsub(".*([0-9A-z]{17}).*","\\1",xmlApply(vin.node, xmlValue)))
  vins = vins[grep("[0-9]", vins)]
  make.node = getNodeSet(doc, "//script[contains(.,'trackingManager.InventoryMakeParam')]")
  make.temp = gsub(".*InventoryMakeParam(.+)trackingManager.InventoryModelParam.*","\\1",xmlApply(make.node, xmlValue))
  make = replicate(toupper(substr(make.temp, gregexpr("'", make.temp, fixed = T)[[1]][1]+1, gregexpr("'", make.temp, fixed = T)[[1]][2]-1)), 
             n = length(vins))
  
  model = "NA"
  trim = "NA"
  year = NA
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
} 

#scrape car information from all the pages
alldata.25 = function(url){
  links = getLinklist.25(url)
  tt = lapply(links, scrapeInfo.25)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#testdata = alldata.25(url)



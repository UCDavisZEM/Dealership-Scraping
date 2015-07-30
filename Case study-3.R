
library(RCurl)
#library(XML)
#test cases
#url = "http://www.balisechevybuickgmc.com/new-inventory/index.htm"
#url = "http://www.lonestarchevrolet.com/new-inventory/index.htm"
#url = "http://www.balisehonda.com/new-inventory/index.htm"
#url = "http://www.davesmith.com/new-inventory/"
#url = "http://www.quirkford.com/new-inventory/?"
#url = "http://www.toyotaofbristol.com/exotic-new-inventory/index.htm"
#url = "http://www.commonwealthchevrolet.com/new-inventory/index.htm"
#url = "https://www.liatoyotaofwilbraham.com/new-inventory/index.htm"
#grab the linklist


getLinklist.3 = function(url){
  xdata = getURLContent(url, useragent = "R")
  doc = htmlParse(xdata, asText = TRUE)
  
  baselink = strsplit(url, "\\?")[[1]][1]
  href = unique(getHTMLLinks(doc))
 
  #pages are obey ?start= pattern
  index = grep("?start=",href, fixed = T)
  
  #number of cars per page
  number = as.numeric(gsub(".*=([0-9]+).*", "\\1", href[index]))
  
  #total number of cars in new inventory
  totalnumber = as.numeric(xpathSApply(doc, "//span[@id='current-search-count']",xmlValue))
  #each pages start number
  startnumber = seq(0, totalnumber-1, number)
  #all the links 
  Linklist = sapply(1:length(startnumber), function(i) paste0(baselink, gsub("[0-9]+", startnumber[i], href[index])))
  return(Linklist)
}



#scrapping data
getdatacontent.3 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}


scrapeInfo.3 <- function(url)
{
  xdata = getURLContent(url, useragent = "R")
  doc = htmlParse(xdata, asText = TRUE)
  vin.node = getNodeSet(doc, "//div[@data-vin]")
  vins = unique(sapply(vin.node,getdatacontent.3, content = "data-vin"))
  #some page have two cars with same vin number
  index = match(vins, sapply(vin.node,getdatacontent.3, content = "data-vin"))
  make = sapply(vin.node,getdatacontent.3, content = "data-make")[index]
  
  model = "NA"
  trim = "NA"
  year = as.numeric(NA)
  
  df <- data.frame(vins,make,model,trim, year, stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
} 

#scrape car information from all the pages
alldata.3 = function(url){
  require(XML)
  links = getLinklist.3(url)
  tt = lapply(links, scrapeInfo.3)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

##cc = alldata.3(url)





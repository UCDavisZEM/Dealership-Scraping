
#library(XML)
#library(RCurl)
#test case
#url = "http://claychevrolet.com/New_Inventory"
#url = "http://buycolonialwest.com/new_inventory"


getLinklist.4 = function(url){
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  href = unique(getHTMLLinks(url))
  tempLinks = href[grep("startRow",href)]
  lastlink = tempLinks[length(tempLinks)]
  tt = strsplit(lastlink, "/")[[1]]
  lastpage.start = as.numeric(tt[length(tt)-2])
  rows.page = as.numeric(tt[length(tt)])
  startrow = seq(1, lastpage.start, rows.page)
  tt[length(tt)-2] = startrow[1]
  Linklist = sapply(1:length(startrow), 
                    function(i) 
                      paste(baselink, paste(tt[2:(length(tt)-3)], collapse = "/"), 
                            startrow[i], 
                            paste(tt[(length(tt)-1):length(tt)], collapse = "/"), 
                            sep = "/"))                    
  return(Linklist)
}

scrapeInfo.4 <- function(url)
{
  doc = htmlParse(url)
  vins = xpathSApply(doc,"//div[@class='vin']",xmlValue)
  make = xpathSApply(doc,"//div[@class='make']",xmlValue)
  model = xpathSApply(doc,"//div[@class='model']",xmlValue)
  trim = xpathSApply(doc,"//div[@class='trim']",xmlValue)
  year = xpathSApply(doc,"//div[@class='vehicleYear']",xmlValue)
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
} 

alldata.4 = function(url){
  links = getLinklist.4(url)
  tt = lapply(links, scrapeInfo.4)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}



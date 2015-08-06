#case study 15
##
#testlinks
#url = "http://www.primetoyotart2.com/inventory/new-vehicles"
#url = "www.primehondacars.com/inventory/new-vehicles"

getLinklist.15 = function(url){
  RSelenium::startServer()
  checkForServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)
  
  
  remDr$navigate(url) 
  
  txt=remDr$getPageSource()
  doc = htmlParse(txt, asText = TRUE)
  baselink =paste0(url, "#0/10/DisplayPrice/a//")
  totalcars = as.numeric(xpathSApply(doc, "//span[@class='resultCount']", xmlValue)[1])
  pages = length(seq(0, totalcars-1, 10))
  
  linklist = sapply(0:(pages-1), function(i)gsub("\\#([0-9]+)", paste0("#", i), baselink))
  remDr$close
  return(linklist)
}

scrapeInfo.15 = function(url){
  RSelenium::startServer()
  checkForServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)
  
  
  remDr$navigate(url) 
  
  txt=remDr$getPageSource()
  doc = htmlParse(txt, asText = TRUE)
  vins = xpathSApply(doc, "//div[@class='vehicleVIN']/span[@class='specValue']", xmlValue)
  
  names = xpathSApply(doc, "//div[@class='vehicleSpecArea']/div[@class='vehicleHeader']", xmlValue)[1:10*2]
  tt = strsplit(names, " ")
  
  make = sapply(tt, "[", 2)
  model = "NA"
  trim = "NA"
  year = sapply(tt, "[", 1)
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  
  remDr$close
  print(url)
  return(df)
}

alldata.15 = function(url){
  require(RSelenium)
  require(XML)
  require(RCurl)
  require(jsonlite)
  links = getLinklist.15(url)
  tt = lapply(links, scrapeInfo.15)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#cc = alldata.15(url)
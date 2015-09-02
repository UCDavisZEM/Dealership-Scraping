#Case study 20
# url = "http://www.wallsford.com/Inventory/?cid=2"

alldata.20 = function(url){
  require(XML)
  require(plyr)
  require(RCurl)
  require(RSelenium)
  RSelenium::startServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)
  
  remDr$navigate(url) 
  Sys.sleep(5)
  doc = list()
  doc[[1]] =remDr$getPageSource()[[1]]
  num = getTotalPage(doc[[1]])
  for(i in 2:num)
  {
    xpathString = paste0("//ul[@class='srp-page-items']/li[contains(.,'",i,"')]")
    webElem <- remDr$findElement(using = "xpath",xpathString)
    webElem$clickElement()
    Sys.sleep(4)
    doc[[i]] = remDr$getPageSource()[[1]]
  }
  cardata = ldply(doc, scrapeInfo.20)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  remDr$close
  return(cardata)
}

getTotalPage <- function(doc)
{
   doc = htmlParse(doc)
   num = max(xpathSApply(doc, "//ul[@class='srp-page-items']/li[@value]/text()", xmlValue))
   return(as.numeric(num))
}

scrapeInfo.20 <- function(doc)
{
  doc = htmlParse(doc)
  vins = unique(xpathSApply(doc, "//li[@class='veh-vin']/span[2]/text()", xmlValue))
  names = xpathSApply(doc, "//h3[@class='vehicle-heading']/text()", xmlValue)
  tt = strsplit(names, " ")    
  make = sapply(tt, "[", 2)
  model = "NA"
  trim = "NA"
  year = sapply(tt, "[", 1)  
  return(data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F))
}

####Case Study 13
#library(RSelenium)
#url = "http://haddadtoyota.com/NewToyotaCars"
#url = "http://foxtoyotaclinton.com/NewToyotaCars"
#url = "http://bmwofvisalia.com/NewBMWCars"
getdatacontent.13 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getLinklist.13 = function(url){
  doc = htmlParse(url)
  
  
  lastpage = xmlAttrs( getNodeSet(doc, "//li[@class='last']/a[@href]")[[1]])["href"]
  TotalPage = as.numeric(gsub(".*Page=([0-9]+)", "\\1", lastpage))
  Linklist = unname(sapply(1:TotalPage, 
                           function(i) 
                             gsub("Page=([0-9]+)", paste0("Page=", i), lastpage)))  
  return(Linklist)
}



scrapeInfo.13 <- function(url)
{
  #print(url)
  require(RSelenium)
  RSelenium::startServer()
  checkForServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open()
  
  remDr$navigate(url)
  Sys.sleep(2)
  #this following approach works
  
  
  
  vin.node = remDr$findElements("xpath", "//img[@class='inventoryPhoto' and @src]")
  temp = sapply(vin.node,function(x)x$getElementAttribute("src"))
  vins = unname(gsub(".*([0-9A-Z]{17}).*", "\\1", temp))
  
  make.node = remDr$findElements("xpath", "//span[@class='field' and @itemprop='name']")
  make = unlist(sapply(make.node, function(x)x$getElementText()))
  model = "NA"
  trim = "NA"
  year = NA
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  
  
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
  
  remDr$close
}

alldata.13 = function(url){
  require(RSelenium)
  require(XML)
  require(RCurl)
  require(jsonlite)
  links = getLinklist.13(url)
  tt = lapply(links, scrapeInfo.13)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#testdata = alldata.13(url)
####Case Study 13
#library(RSelenium)
#url = "http://haddadtoyota.com/NewToyotaCars"
#url = "http://foxtoyotaclinton.com/NewToyotaCars"
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
  
  #this following approach works
  RSelenium::startServer()
  checkForServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)
  remDr$navigate(url) 
  
  txt=remDr$getPageSource()
  doc = htmlParse(txt, asText = TRUE)
  
  vin.node = getNodeSet(doc, "//img[@class='inventoryPhoto' and @src]")
  temp = sapply(vin.node,getdatacontent.3, content = "src")
  vins = unname(gsub(".*([0-9A-Z]{17}).*", "\\1", temp))
  make = xpathSApply(doc, "//span[@class='field' and @itemprop='name']", xmlValue)
  model = xpathSApply(doc, "//span[@class='field' and @itemprop='model']", xmlValue)
  trim = xpathSApply(doc, "//span[@class='field' and @itemprop='description']", xmlValue)
  year = xpathSApply(doc, "//span[@class='field' and @itemprop='releaseDate']", xmlValue)
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  remDr$close
  return(df)  
}

url="http://haddadtoyota.com/NewToyotaCars?Page=1"

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





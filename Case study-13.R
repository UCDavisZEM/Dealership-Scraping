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



scrapeInfo.13 <- function(txt)
{
  
  #this following approach works  
  doc = htmlParse(txt, asText = TRUE)
  
  vin.node = getNodeSet(doc, "//img[@class='inventoryPhoto' and @src]")
  temp = sapply(vin.node,getdatacontent.3, content = "src")
  vins = unname(gsub(".*([0-9A-Z]{17}).*", "\\1", temp))
  make = xpathSApply(doc, "//span[@class='field' and @itemprop='name']", xmlValue)
  model = xpathSApply(doc, "//span[@class='field' and @itemprop='model']", xmlValue)
  trim = NA
  year = NA
  df <- data.frame(vins,make,model,trim,year, stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  
  return(df)  
}


alldata.13 = function(url){
  require(XML)
  require(RCurl)
  require(jsonlite)
  require(RSelenium)
  
  RSelenium::startServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)
  links = getLinklist.13(url)
  tt = list()
  #tt = lapply(links, scrapeInfo.13)
  for(url in links)
  {
    remDr$navigate(url)    
    Sys.sleep(4)
    txt=remDr$getPageSource()
    tt[[url]] = scrapeInfo.13(txt)
  }
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  remDr$close
  return(cardata)
}

#testdata = alldata.13(url)





####Case Study 13
#library(RSelenium)

#url = "http://haddadtoyota.com/NewToyotaCars"
#url = "http://foxtoyotaclinton.com/NewToyotaCars"

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
  require(RSelenium)
  RSelenium::startServer()
  checkForServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open()
  remDr$navigate(url)
  
  #this following approach works
  txt=remDr$getPageSource()
  doc = htmlParse(txt, asText = TRUE)
  xpathSApply(doc, "//span[@class='field' and @itemprop='name']", xmlValue)
 
  vins = xpathSApply(doc, "//span[@class='field' and @itemprop='productID']", xmlValue)
  make = xpathSApply(doc, "//span[@class='field' and @itemprop='name']", xmlValue)
  model = xpathSApply(doc, "//span[@class='field' and @itemprop='model']", xmlValue)
  trim = xpathSApply(doc, "//span[@class='field' and @itemprop='description']", xmlValue)
  year = xpathSApply(doc, "//span[@class='field' and @itemprop='releaseDate']", xmlValue)
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





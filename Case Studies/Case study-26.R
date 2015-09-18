#Case study 26
#url = "http://bmw.niello.com/new.php"

scrapeInfo.26 <- function(url)
{
  doc = htmlParse(url) 
  #vin node
  vinnode = getNodeSet(doc,"//div[@id='detaildetailscolumn']/text()")[[3]]
  vin = xmlValue(vinnode,trim=T)
  
  #title node
  titlenode = getNodeSet(doc,"//h1[@itemprop='name']/text()")[[3]]
  titleStr = xmlValue(titlenode,trim=T)
  year = gsub('([0-9]{4}).*','\\1',titleStr)
  make = gsub('[0-9]{4} (.*?) .*','\\1',titleStr)
  trim = NA
  model = NA
  
  df <- data.frame(vin,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
}

alldata.26 = function(url){
  require(RSelenium)
  require(XML)
  require(plyr)
  
  RSelenium::startServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)  
  remDr$navigate(url)
  
  Sys.sleep(3)
  doc = remDr$getPageSource()[[1]]
  
  remDr$close()
  
  doc = htmlParse(doc)
  links = getHTMLLinks(doc)
  carlinks = unique(links[grep("/details\\.php",links)])
  cardata = ldply(carlinks,scrapeInfo.26)
  return(cardata)
}
  
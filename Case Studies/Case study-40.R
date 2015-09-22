#Case study 40
#url = "http://www.bmwoffreehold.com/browse-new-bmw-inventory-in-nj" 

alldata.40 = function(url){
  require(RSelenium)
  require(XML)
  require(plyr)
  RSelenium::startServer()
  checkForServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)  
  remDr$navigate(url)
  Sys.sleep(2)
  #find the total count number and the number on each page
  countElem = remDr$findElement('xpath','//li[@class="last"]/span/a')
  countStr = unlist(countElem$getElementAttribute("href"))
  count = as.numeric(gsub(".*Page=([0-9]+)$","\\1",countStr))
  
  #times to hit the next page element 
  times = count-1
  
  #nextElem$highlightElement()
  allcars = list()
  i=0
  for(p in seq(1:(times+1)))
  {
    Sys.sleep(2)
    i = i+1
    #find the title element which contains the make and year information
    titleElems = remDr$findElements("xpath","//div[@class='text-fields column']/a")
    titleStrs = unlist(lapply(titleElems, function(x){x$getElementAttribute("href")}))
    
    years = gsub(".*/([0-9]{4})/.*","\\1",titleStrs)
    make = gsub(".*/[0-9]{4}/(.*?)/.*","\\1",titleStrs)
    
    #find the vin element 
    vins = gsub(".*/([0-9A-Z]{17})/.*","\\1",titleStrs)
    
    model = "NA"
    trim = "NA"
    
    allcars[[i]] = data.frame(vins,make,model,trim,as.numeric(years), stringsAsFactors = F)
    
    if(p<=times)
    {
      #find the next page element and click to the next page
      nextElem = remDr$findElement("xpath","//span[@class='pager-nextpage']/a") 
      nextElem$clickElement()
    }
  }
  remDr$close()
  df = ldply(1:length(allcars), function(i) {allcars[[i]]})
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
}
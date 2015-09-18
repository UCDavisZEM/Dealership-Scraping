#Case study 28
#url = "http://www.bmwbayside.com/inventory/new/all/bmw/all/all/all"
#url = "http://www.bmwmainline.com/inventory/new/all/bmw/all/all/all"

alldata.28 = function(url){
    require(RSelenium)
    require(XML)
    require(plyr)
    RSelenium::startServer()
    checkForServer()
    remDr = remoteDriver(browserName = "firefox")
    remDr$open(silent = TRUE)  
    remDr$navigate(url)
    
    #find the total count number and the number on each page
    countElem = remDr$findElement('xpath','//div[@class="pull-left"]/small/strong')
    countStr = unlist(countElem$getElementText())
    num = as.numeric(gsub(".*([0-9]{2}?).*([0-9]+).*","\\1",countStr))
    count = as.numeric(gsub(".*([0-9]{2}).* ([0-9]+) .*","\\2",countStr))
    
    #times to hit the next page element 
    times = floor(count/num)
    
    #nextElem$highlightElement()
    allcars = list()
    i=0
    for(p in seq(1:(times+1)))
    {
      Sys.sleep(2)
      i = i+1
      #find the title element which contains the make and year information
      titleElems = remDr$findElements("xpath","//h3[@class='mt-5 vehicle-title']")
      titleStrs = unlist(lapply(titleElems, function(x){x$getElementText()}))
      
      years = gsub("([0-9]{4}).*","\\1",titleStrs)
      make = gsub("[0-9]{4} (.*?) .*","\\1",titleStrs)
      
      #find the vin element 
      vinElems = remDr$findElements("xpath","//div[@class='modal fade text-left']")
      vinStrs = unlist(lapply(vinElems, function(x){x$getElementAttribute("id")}))
      vins = gsub(".*-([0-9A-Z]{17})","\\1",vinStrs)
      
      model = "NA"
      trim = "NA"
      
      allcars[[i]] = data.frame(vins,make,model,trim,as.numeric(years), stringsAsFactors = F)
      
      if(p<=times)
      {
        #find the next page element and click to the next page
        nextElem = remDr$findElement("xpath","//small[@class='pull-right hidden-xs']/small[contains(.,'>>')]") 
        nextElem$clickElement()
      }
    }
    remDr$close()
    df = ldply(1:length(allcars), function(i) {allcars[[i]]})
    colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
    return(df)
}
#Case Study 29
#pattern: newinventory.aspx
#url = "http://www.dchhondaofnanuet.com/newinventory.aspx"
#url = "http://www.bmwofbloomfield.com/newinventory.aspx"

alldata.29 = function(url){
  RSelenium::startServer()
  checkForServer()
  
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)  
  remDr$navigate(url)
  showallelement = remDr$findElement("xpath","//option[@value='All']")
  
  
  showallelement$clickElement()
  
  Sys.sleep(2)
  #this following approach works
  
  
  
  vin.node = remDr$findElements("xpath", "//div[@class='rowcon' and contains(.,'VIN')]")
  temp = sapply(vin.node,function(x)x$getElementText())
  vins = unname(gsub(".*([0-9A-Z]{17}).*", "\\1", temp))
  
  name.node = remDr$findElements("xpath", "//img[@width='240' and @alt]")
  name = unlist(sapply(name.node, function(x)x$getElementAttribute("alt")))
  tt = strsplit(name, " ")
  make = sapply(tt, "[", 2)
  model = "NA"
  trim = "NA"
  year = NA
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  df
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
  
  remDr$close
}

#car = alldata.29(url)

#url = "http://www.chapmanbmwoncamelback.com/bmw-cars.asp"

getdatacontent.24 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getLinklist.24 = function(url){
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3])
  doc = htmlParse(url)
  model.linknode = getNodeSet(doc, "//a[@class='model']")
  model.links = paste0(baselink, unname(sapply(model.linknode, getdatacontent.24, content = "href")), "#?pageSize=50")
}

getallcarlinks.24 = function(url){ 
  remDr$navigate(url)
  
  countElem = remDr$findElements('xpath','//span[@class="blue ng-binding"]')
  count = as.numeric(unlist(lapply(countElem, function(x){x$getElementText()})))
  pages = floor(count/50)
  
  pagelinks = sapply(0:pages,function(num) paste0(url,'&currentPage=',num))
  
  if(length(pagelinks)==1){
    webElems = remDr$findElements('xpath','//div[@class="topLine clear"]/a[1]')
    carlinks = unlist(lapply(webElems, function(x){x$getElementAttribute("href")}))
  }
  else{
    carlinks = list()
    i = 0
    for(link in pagelinks){
      i = i+1
      remDr$navigate(link)
      webElems = remDr$findElements('xpath','//div[@class="topLine clear"]/a[1]')
      carlinks[[i]] = unlist(lapply(webElems, function(x){x$getElementAttribute("href")}))      
    }
    carlinks = unlist(lapply(1:length(carlinks), function(i) {carlinks[[i]]}))
  } 
  return(carlinks)
}

scrapeInfo.24 = function(url){
  doc = htmlParse(url)
  vinNodes = getNodeSet(doc,path = '//form[@class="detailForm"]/input[@name="vin"]')
  vin = xmlSApply(vinNodes, xmlGetAttr, name='value')
  
  makeNodes = getNodeSet(doc,path='//form[@class="detailForm"]/input[@name="make"]')
  make = xmlSApply(makeNodes, xmlGetAttr, name='value')
  
  yearNodes =  getNodeSet(doc,path = '//form[@class="detailForm"]/input[@name="year"]')
  year = xmlSApply(yearNodes, xmlGetAttr, name='value')
  
  model = "NA"
  trim = "NA"
  df <- data.frame(vin,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  free(doc)
  return(df)
}

alldata.24 = function(url){
  require(RSelenium)
  require(XML)
  require(plyr)
  links = getLinklist.24(url)
  
  RSelenium::startServer()
  checkForServer()
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)  
  allcarlinks = list()
  i = 0 
  for(carlink in links) {
    i = i+1
    remDr$navigate(carlink)
    Sys.sleep(2)
    
    countElem = remDr$findElements('xpath','//span[@class="blue ng-binding"]')
    count = as.numeric(unlist(lapply(countElem, function(x){x$getElementText()})))
    pages = floor(count/50)
    
    pagelinks = sapply(0:pages,function(num) paste0(carlink,'&currentPage=',num))
    
    if(length(pagelinks)==1){
      webElems = remDr$findElements('xpath','//div[@class="topLine clear"]/a[1]')
      carlinks = unlist(lapply(webElems, function(x){x$getElementAttribute("href")}))
    }
    else{
      carlinks = list()
      p = 0
      for(link in pagelinks){
        p = p+1
        remDr$navigate(link)
        webElems = remDr$findElements('xpath','//div[@class="topLine clear"]/a[1]')
        carlinks[[p]] = unlist(lapply(webElems, function(x){x$getElementAttribute("href")}))      
      }
      carlinks = unlist(lapply(1:length(carlinks), function(i) {carlinks[[i]]}))
    } 
    #print(carlinks)
    allcarlinks[[i]] = carlinks
  }
  allcarlinks = unlist(lapply(1:length(allcarlinks), function(i) {allcarlinks[[i]]}))
  
  cardata = ldply(allcarlinks, scrapeInfo.24)
  remDr$close()
  return(cardata)
}
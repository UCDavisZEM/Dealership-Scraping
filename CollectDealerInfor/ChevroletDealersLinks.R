#Collect all the nation-wide Chevrolet dealerships with information about name, address, website, inventory link and geo location
install.packages("XML", repos = "http://cran.cnr.Berkeley.edu/")
library(XML)
install.packages("plyr", repos = "http://cran.cnr.Berkeley.edu/")
library(plyr)

load("zipdata.rdata")
cities = unique(zipdata$city)
#length(cities)

cities = gsub(' ','',cities)
#eg. url = "http://www.chevydealer.com/SanDiego/dealers"
allsearchpages = unname(sapply(cities, function(city_str) paste0("http://www.chevydealer.com/", city_str,"/dealers")))

getdealerinfo<- function(url){
 # print(url) 
  tryCatch({
      doc = htmlParse(url)
      dealerNameNodes = getNodeSet(doc,'//div[@class="dealer-name-and-address"]/a[@class="dealer-name"]/span/text()')
      dealerName = xmlSApply(dealerNameNodes,xmlValue,trim=T)
      
      roadNodes = getNodeSet(doc,'//div[@class="dealer-name-and-address"]/div[1]/text()')
      roadName = xmlSApply(roadNodes,xmlValue,trim=T)[-1]
      
      cityNodes = getNodeSet(doc,'//div[@class="cityStateZip"]/span[1]/text()')
      cityName = xmlSApply(cityNodes,xmlValue,trim=T)
      
      stateNodes = getNodeSet(doc,'//div[@class="cityStateZip"]/span[2]/text()')
      stateName = xmlSApply(stateNodes,xmlValue,trim=T)
      
      zipcodeNodes = getNodeSet(doc,'//div[@class="cityStateZip"]/span[3]/text()')
      zipcode = xmlSApply(zipcodeNodes,xmlValue,trim=T)
      
      dealerAddress = paste0(roadName,', ',cityName,', ',stateName,' ',zipcode)
      
      dealerWebsiteNodes = getNodeSet(doc,"//div[@class='dealer-name-and-address']/a")
      dealerWebsite = unlist(lapply(xmlSApply(dealerWebsiteNodes,xmlGetAttr,"href")[-c(1,2)],gsub,patter='(.*/).*',replacement='\\1'))
      
      dealerIVwebsiteNodes = getNodeSet(doc,"//a[contains(./text(),'View Inventory')]")
      dealerIVWebsite = unlist(lapply(xmlSApply(dealerIVwebsiteNodes,xmlGetAttr,"href"),gsub,patter='(.*)referrer.*',replacement='\\1'))
      dealerInventoryLink = paste0(dealerIVWebsite,'search=new')
      
      GeoNodes = getNodeSet(doc,'//div[@class="dealer-listing-item"]')
      Latitude = xmlSApply(GeoNodes,xmlGetAttr,"data-latitude")
      Longitude = xmlSApply(GeoNodes,xmlGetAttr,"data-longitude")
      
      df <- data.frame(dealerName, dealerAddress, dealerWebsite, zipcode, dealerInventoryLink, Latitude, Longitude, stringsAsFactors=F )
      colnames(df) = c("Dealer","Address","Link","zipcode","IV_link", "Latitude", "Longitude")
      return(df)
  }, error = function(err){
    return()
  })
}
#url2 = "http://www.chevydealer.com/NewYork/dealers"

chevroletDealers = ldply(allsearchpages, function(url){
                                                      out = try(getdealerinfo(url))
                                                      if(class(out)=='try-error') next;
                                                      return(out)
                                                      }, .progress = "text" )
ChevroletDealers = chevroletDealers[!duplicated(chevroletDealers$Dealer),]
ChevroletDealers = merge(ChevroletDealers, zipdata)
save(ChevroletDealers, file="chevroletDealers.rdata")


head(ChevroletDealers)

=============================#further cleaning
require(RSelenium)
RSelenium::startServer()
remDr = remoteDriver(browserName = "firefox")
remDr$open(silent = TRUE)

getDealerInforInRS = function(doc){
  doc = htmlParse(doc)
  dealerNameNodes = getNodeSet(doc,'//div[@class="dealer-name-and-address"]/a[@class="dealer-name"]/span/text()')
  dealerName = xmlSApply(dealerNameNodes,xmlValue,trim=T)
  
  roadNodes = getNodeSet(doc,'//div[@class="dealer-name-and-address"]/div[1]/text()')
  roadName = xmlSApply(roadNodes,xmlValue,trim=T)[-c(1,2,3,4)]
  
  cityNodes = getNodeSet(doc,'//div[@class="cityStateZip"]/span[1]/text()')
  cityName = xmlSApply(cityNodes,xmlValue,trim=T)
  
  stateNodes = getNodeSet(doc,'//div[@class="cityStateZip"]/span[2]/text()')
  stateName = xmlSApply(stateNodes,xmlValue,trim=T)
  
  zipcodeNodes = getNodeSet(doc,'//div[@class="cityStateZip"]/span[3]/text()')
  zipcode = xmlSApply(zipcodeNodes,xmlValue,trim=T)
  
  dealerAddress = paste0(roadName,', ',cityName,', ',stateName,' ',zipcode)
  
  dealerWebsiteNodes = getNodeSet(doc,"//div[@class='dealer-name-and-address']/a")
  dealerWebsite = unlist(lapply(xmlSApply(dealerWebsiteNodes,xmlGetAttr,"href")[-c(1,2)],gsub,patter='(.*/).*',replacement='\\1'))
  
  dealerIVwebsiteNodes = getNodeSet(doc,"//a[contains(./text(),'View Inventory')]")
  dealerIVWebsite = unlist(lapply(xmlSApply(dealerIVwebsiteNodes,xmlGetAttr,"href"),gsub,patter='(.*)referrer.*',replacement='\\1'))
  dealerInventoryLink = paste0(dealerIVWebsite,'search=new')
  
  GeoNodes = getNodeSet(doc,'//div[@class="dealer-listing-item"]')
  Latitude = xmlSApply(GeoNodes,xmlGetAttr,"data-latitude")
  Longitude = xmlSApply(GeoNodes,xmlGetAttr,"data-longitude")
  
  df <- data.frame(dealerName, dealerAddress, dealerWebsite, zipcode, dealerInventoryLink, Latitude, Longitude, stringsAsFactors=F )
  colnames(df) = c("Dealer","Address","Link","zipcode","IV_link", "Latitude", "Longitude")
  return(df)
}
remDr$navigate(url) 

dataset = ldply(aa, .progress = T, function(zip){ 
    print(zip)
    webElem <- remDr$findElement(using = 'id',value="zip")
    webElem$sendKeysToElement(list(zip,"\uE007"))
    doc <- remDr$getPageSource()[[1]]
    return(getDealerInforInRS(doc))
})


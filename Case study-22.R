#Case study 22
#url = "http://www.toyotaofbraintree.com/new-cars/for-sale"

alldata.22 = function(url){
  require(XML)
  require(plyr)
  links = getLinklist.22(url)
  cardata = ldply(links, scrapeInfo.22)
  return(cardata)
}

getLinklist.22 = function(url){
  doc = htmlParse(url)
  baselink = paste0(url, "?startrow=1")
  #total number of cars in new inventory
  countNode = xmlValue(getNodeSet(doc, "//span[contains(.,'results')]/text()")[[1]],trim = T)
  totalnumber = as.numeric(gsub('.*of.*?([0-9]+) results','\\1',countNode))
  #each pages start number
  startnumber = seq(1, totalnumber-1, 25)
  #all the links 
  Linklist = sapply(startnumber, function(num) gsub('\\d+',num,baselink))
  return(Linklist)
}

scrapeInfo.22<-function(url)
{
  #print(url)
  #url = "http://www.balisetoyota.com/web/new/Toyota-4Runner-2015-West-Springfield-Massachusetts/20934167/?condition_id=10425"
  doc = htmlParse(url)
  vin = xpathSApply(doc,'//div[contains(@class,"vehicle-item")]',xmlGetAttr,'data-vin')
  year = xpathSApply(doc,'//div[contains(@class,"vehicle-item")]',xmlGetAttr,'data-year')
  make = xpathSApply(doc,'//div[contains(@class,"vehicle-item")]',xmlGetAttr,'data-carmake')
  model = xpathSApply(doc,'//div[contains(@class,"vehicle-item")]',xmlGetAttr,'data-carmodel')
  trim = "NA"
  
  return(data.frame(vin,make,model,trim,as.numeric(year), stringsAsFactors = F))
}
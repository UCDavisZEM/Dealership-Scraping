#Case study 32
#url = "http://bmwofbridgewater.com/vehicle/inventory/new"

getLinklist.32 = function(url){
  doc = htmlParse(url)
  baselink = gsub('(.*com).*','\\1',url)
  countNode = xmlValue(getNodeSet(doc,"//p[@class='pagination-page-summary']/text()")[[2]],trim=T)
  count = as.numeric(gsub(".* ([0-9]+)? .*","\\1",countNode))
  
  numNode = xmlValue(getNodeSet(doc,"//p[@class='pagination-page-summary']/strong/text()")[[1]],trim=T)
  num = as.numeric(gsub(".* ([0-9]+)?",'\\1',numNode))
  
  pages = ceiling(count/num)
  links = unname(sapply(1:pages, function(i) paste0(baselink,'/vehicle/inventory?condition=New&pg=',i)))
  return(links)
}

scrapeInfo.32 =function(url){
  doc = htmlParse(url)
  links = getHTMLLinks(doc)
  carlinks = unique(links[grep('com/cars/new-',links)])
  
  vins = gsub('.*([0-9A-Z]{17})$','\\1',carlinks)
  year = gsub('.*([0-9]{4}?).*','\\1',carlinks)
  make = gsub('.*[0-9]{4}-(.*?)-.*','\\1',carlinks)
  model = "NA"
  trim = "NA"
  
  return(data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F))
}

alldata.32 = function(url){
  require(XML)
  require(plyr)
  links = getLinklist.32(url)
  cardata = ldply(links, scrapeInfo.32)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(cardata)
}
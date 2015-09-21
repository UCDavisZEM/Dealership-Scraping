#Case study 30
#url = "http://www.sansonestoyota.com/new.cfm"

getLinklist.30 = function(url){
  doc = htmlParse(url)
  baselink = gsub('(.*com).*','\\1',url)
  countNode = xmlValue(getNodeSet(doc,"//div[@class='pagination']/text()")[[2]],trim=T)
  count = as.numeric(gsub(".* ([0-9]+)?.*","\\1",countNode))
  
  numNode = xmlValue(getNodeSet(doc,"//div[@class='pagination']/b/text()")[[1]],trim=T)
  num = as.numeric(gsub(".*- ([0-9]+)?",'\\1',numNode))
  
  pages = ceiling(count/num)
  links = unname(sapply(1:pages, function(i) paste0(baselink,'/new.cfm/sheet/',i,'/#sort')))
  return(links)
}

scrapeInfo.30 = function(url){
  doc = htmlParse(url)
  links = getHTMLLinks(doc)
  carlinks = unique(links[grep('/new\\.cfm/vehicle/',links)])
  
  vins = gsub('.*([0-9A-Z]{17})/','\\1',carlinks)
  year = gsub('.*([0-9]{4}?).*','\\1',carlinks)
  make = gsub('.*[0-9]{4}-(.*?)-.*','\\1',carlinks)
  model = "NA"
  trim = "NA"
  
  return(data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F))
  
}

alldata.30 = function(url){
  require(XML)
  require(plyr)
  links = getLinklist.30(url)
  cardata = ldply(links, scrapeInfo.30)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(cardata)
}
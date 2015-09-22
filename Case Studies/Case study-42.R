#Case study 42
#url = "http://www.elmhurstbmw.com/inventory-results?selectedcat=new"

getLinklist.42 = function(url){
  doc = htmlParse(url)
  baselink = gsub('(.*com).*','\\1',url)
  countNode = xmlValue(getNodeSet(doc,"//div[@class='vehicle-count']/b/text()")[[1]],trim=T)
  count = as.numeric(gsub(".* ([0-9]+)? .*","\\1",countNode))
  
  numNode = getNodeSet(doc, '//select[@id="resultsPerPage"]/option/text()')
  nums =  xmlSApply(numNode,xmlValue,trim=T)
  numperpage = max(as.numeric(nums))
  
  pages = ceiling(count/numperpage)
  links = unname(sapply(1:pages, function(i) paste0(baselink,'/inventory-results?selectedcat=new&vperpage=',numperpage,'&page=',i)))
  return(links)
}

scrapeInfo.42 = function(url){
  doc = htmlParse(url)
  links = getHTMLLinks(doc)
  carlinks = unique(links[grep('/inventory-details/title/',links)])
  year = gsub('.*([0-9]{4}?).*','\\1',carlinks)
  make = gsub('.*[0-9]{4}-(.*?)-.*','\\1',carlinks)
  
  vinNodes = getNodeSet(doc,"//ul[@class='feature-list alternate clearfix']/li[1]/span/text()")
  vins = xmlSApply(vinNodes,xmlValue,trim=T)
  
  model = "NA"
  trim = "NA"
  
  return(data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F))
}

alldata.42 = function(url){
  require(XML)
  require(plyr)
  links = getLinklist.42(url)
  cardata = ldply(links, scrapeInfo.42)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(cardata)
}
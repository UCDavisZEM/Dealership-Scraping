#Case study 16
# url = "http://route44toyota.com/New-Inventory.aspx"
# url = "http://www.watertownford.com/new-inventory.aspx" (different pattern)

alldata.16 = function(url){
  require(XML)
  require(plyr)
  links = getLinklist.16(url)
  cardata = ldply(links, scrapeInfo.16)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(cardata)
}

getLinklist.16 = function(url){
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)

  count = xmlValue(getNodeSet(doc,'//span[@class="PagingNumberFound"]/text()')[[1]],trim=T)
  pagenum = ceiling(as.numeric(count)/25)  
  
  baselink = paste0(url, "?currentpage=1")
  linklist = sapply(1:pagenum,function(i) gsub("currentpage=([0-9]+)", paste0("currentpage=",i), baselink))
  return(linklist)
}

scrapeInfo.16 = function(url){
  #print(url)
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  links = getHTMLLinks(doc)
  contentlinks = unique(links[grep('[0-9A-Z]{17}.aspx$',links)])
  vins = gsub('.*([0-9A-Z]{17}).*','\\1',contentlinks)
  year = gsub('.*/([0-9]{4}).*','\\1',contentlinks)
  make = gsub('.*[0-9]{4}_(.*?)_.*','\\1',contentlinks)
  model = "NA"
  trim = "NA"
  
  return(data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F))
}


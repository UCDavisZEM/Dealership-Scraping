###
#case study 33
#pattern : /new/inventory/
#url = "http://www.paulmillerbmw.com/new/inventory"
#url = "http://www.paulmillerhonda.com/new/inventory"
#url = "http://www.paulmillerbmw.com/new/inventory"

##index/list/price/asc/15


getdatacontent.33 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getLinklist.33 = function(url){
    txt = getURLContent(url, useragent = "R")
    doc = htmlParse(txt, asText = TRUE)
    last.node = getNodeSet(doc, "//a[contains(.,'Last')]")
   lastpagelink = sapply(last.node,getdatacontent.33, content = "href")
    index = seq(15, as.numeric(gsub(".*asc/([0-9]+).*", "\\1", lastpagelink)), 15)
    linklist = sapply(1:length(index), function(i)gsub(index[length(index)], index[i], lastpagelink))
   linklist = c(gsub(index[length(index)], "", lastpagelink), linklist)
   return(linklist)
}

scrapeInfo.33 <- function(url){
  doc = htmlParse(url)
  nodes = getNodeSet(doc, "//div[@class='comparecheck']/div[@rel]")
  carinfo = unique(sapply(nodes,getdatacontent.33, content = "rel"))
  carinfo.temp   = sapply(carinfo, fromJSON, simplifyVector = T, simplifyDataFrame = T)
  vins.temp = unlist(unname(carinfo.temp[1, 1:dim(carinfo.temp)[2]]))
  vins = sapply(strsplit(vins.temp, "_"), "[", 2)
  name = unlist(unname(carinfo.temp[2, 1:dim(carinfo.temp)[2]]))
  make = sapply(strsplit(name, " "), "[", 2)
  model = "NA"
  trim = "NA"
  year = NA
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
} 


#scrape car information from all the pages
alldata.33 = function(url){
  links = getLinklist.33(url)
  tt = lapply(links, scrapeInfo.33)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}
#url = "http://www.paulmillerhonda.com/new/inventory"
#url = "http://www.paulmillerbmw.com/new/inventory/index"

#testdata = alldata.25(url)




#library(XML)
#library(RCurl)
#test case
#url = "http://www.libertychev.com/searchnew.aspx"
#url = "http://www.bernardihondanatick.com/searchnew.aspx"
#url = "http://www.nissanvillage.com/searchnew.aspx"

#grab the linklist

getLinklist.6 = function(url){
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  lastlink.node = getNodeSet(doc, "//div[@id='Lastbutton']/a[@href]")[[1]]
  if(length(lastlink.node)==0)
  {
    lastlink.node = getNodeSet(doc,"//text()[contains(.,'Last')]/..")[[1]]
    lastlink = xmlAttrs(lastlink.node)["href"]
  }
  else{
    lastlink = xmlAttrs(lastlink.node)["href"]
  }
  totalpage = as.numeric(gsub(".*=([0-9]+).*", "\\1", lastlink))
  Linklist = sapply(1:totalpage, function(i) paste(baselink, gsub("[0-9]+", i, lastlink), sep = "/"))         
  return(Linklist)
}


scrapeInfo.6 <- function(url)
{
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  vin_nodes = getNodeSet(doc, "//li[@class='vinDisplay']/text()")
  vins = unlist(lapply(vin_nodes,xmlValue))
  #tt = strsplit(name, " ")
  make = "NA"
  model = "NA"
  trim = "NA"
  year = "NA"
  
  df <- data.frame(vins,make,model,trim,year, stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
} 

#scrape car information from all the pages
alldata.6 = function(url){
  require(XML)
  require(RCurl)
  links = getLinklist.6(url)
  tt = lapply(links, scrapeInfo.6)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#testdata = alldata.6(url)

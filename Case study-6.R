
#library(XML)
#library(RCurl)
#test case
#url = "http://www.libertychev.com/searchnew.aspx"
#url = "http://www.bernardihondanatick.com/searchnew.aspx"


#grab the linklist

getLinklist.6 = function(url){
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  lastlink = xmlAttrs(getNodeSet(doc, "//div[@id='Lastbutton']/a[@href]")[[1]])["href"]
  totalpage = as.numeric(gsub(".*=([0-9]+).*", "\\1", lastlink))
  Linklist = sapply(1:totalpage, function(i) paste(baselink, gsub("[0-9]+", i, lastlink), sep = "/"))         
  return(Linklist)
}

getdatacontent.6 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

scrapeInfo.6 <- function(url)
{
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  nodes = getNodeSet(doc, "//a[@id='sendToMobileLink']")
  vins = unique(sapply(nodes,getdatacontent.6, content = "vin"))
  name = sapply(nodes,getdatacontent.6, content = "ref")
  tt = strsplit(name, " ")
  make = sapply(tt, "[", 2)
  model = "NA"
  trim = "NA"
  year = sapply(tt, "[", 1)
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
} 

#scrape car information from all the pages
alldata.6 = function(url){
  links = getLinklist.6(url)
  tt = lapply(links, scrapeInfo.6)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

testdata = alldata.6(url)

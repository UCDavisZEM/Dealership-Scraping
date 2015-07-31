#Case 14
# url = "http://www.copelandtoyota.com/NewToyotaCars.aspx"

#grab the linklist

getdatacontent.14 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getLinklist.14 = function(url){
  require(RCurl)
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  linkNodes = getNodeSet(doc, "//span[@id='BottomPaging']/a[@href]")  
  links = unname(sapply(linkNodes,getdatacontent.14,content='href'))    
  totalpage = max(as.numeric(gsub(".*=([0-9]+?).*", "\\1", links)))
  Linklist = sapply(1:totalpage, function(i) paste(baselink, sub("[0-9]+", i, links[1]), sep = "/"))         
  return(Linklist)
}

scrapeInfo.14 <- function(url)
{
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  table = readHTMLTable(doc)[[1]][,c(-1,-6,-7,-8,-9)] #with make,year,model information
  colnames(table)<-c('Year','Make','Model','Trim')
  #to get vins
  links = getHTMLLinks(doc)
  links = unique(links[grep('[0-9A-Z]{17}.aspx',links)])
  vins = gsub('.*([0-9A-Z]{17}).*','\\1',links)
  
  df <- data.frame(vins,table$Make,table$Model,table$Trim,table$Year, stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
} 

#scrape car information from all the pages
alldata.14 = function(url){
  require(XML)
  links = getLinklist.14(url)
  tt = lapply(links, scrapeInfo.14)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}
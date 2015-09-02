####Case study 11
##Case pattern:/new-cars-haverhill-ma
#test cases
#url = "http://www.autofairfordma.com/new-cars-haverhill-ma"
#url = "http://www.rodmanford.com/new-cars-boston-ma"
#library(XML)
#library(jsonlite)

getLinklist.11 = function(url){
  
  doc = htmlParse(url)
  #the reason is the second test link...when you go page 2 it just turned out to be another website....
  #so I use this way to get the "baselink"
  baselink = unname(xmlAttrs(
    getNodeSet(doc, "//li[@id='il-pagination-element-1']/a[@href]")[[1]])['href'])
  
  temp = xpathSApply(doc, "//script[@type='text/javascript']",xmlValue)
  index =  grep("lastPage",temp)[1]
  tt = gsub("\\s+", "", temp[index])
  totalpage = as.numeric(gsub(".*lastPage=([0-9]+).*", "\\1", tt))
  
  Linklist = unname(sapply(1:totalpage, 
                           function(i) paste0(baselink,"?page=", i)))  
  
  return(Linklist)
}

scrapeInfo.11 <- function(url)
{
doc = htmlParse(url)
txt = xpathSApply(doc, "//script[@type='text/javascript']",xmlValue)
tt =  grep("vehicleListGaObjects",txt)
temptxt = substr(txt[tt], 85, nchar(txt[tt])-10)
data = fromJSON(temptxt, simplifyVector = T, simplifyDataFrame = TRUE)
cardata = Reduce(function(x, y) merge(x, y, all=T), data)
df <- data.frame(as.character(cardata$VIN),as.character(cardata$Make),as.character(cardata$Model),as.character(cardata$Trim),as.numeric(as.character(cardata$Year)), stringsAsFactors = F)
colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
return(df)
}

alldata.11 = function(url){
  links = getLinklist.11(url)
  tt = lapply(links, scrapeInfo.11)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#test = alldata.11(url)

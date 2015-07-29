library(XML)
##the only input url
#url = "http://www.hanleesdavischevy.com/VehicleSearchResults?search=new&pageNumber=1"
#url = "http://www.classicchevrolet.com/VehicleSearchResults?search=new"
#url  = "http://www.centralchevyauto.com/VehicleSearchResults?search=new"
doc = htmlParse(url)
#baselink = xmlToList(doc, addAttributes = FALSE)[[1]]$base[1]
#for try: http://www.gmautoplaza.com/VehicleSearchResults?search=new


#grab the next link


getNextlink.1 = function(url, baselink){
  href = unique(getHTMLLinks(url))
  temp = href[grep("pageNumber",href)]
  nextLink = paste0(baselink, temp[length(temp)])
  return(nextLink)
}

#function to get all the links  
getLinklist.1 = function(originalurl){

  #get base link and the seperate the originalurl
  baselink = substr(originalurl, 1, gregexpr("/",originalurl)[[1]][3])
  href = unique(getHTMLLinks(originalurl))
  temp = href[grep("pageNumber",href)]
  
  if(length(temp) == 0){
    linklist = originalurl
  }
  else{
    #get the first href
    firstpagelink = paste0(baselink, gsub("2", "1", temp))
    linklist = firstpagelink
    #grab next links
    nextlink  = getNextlink.1(originalurl, baselink)
    linklist = c(linklist, nextlink, getNextlink.1(nextlink, baselink))
    while(linklist[length(linklist)]!=linklist[length(linklist)-2]){
      newlink = getNextlink.1(linklist[length(linklist)], baselink)
      #print(newlink)
      linklist = c(linklist, newlink)
    }
    linklist = linklist[1:c(length(linklist)-1)]
  }
  
  return(linklist)
}


#scrap all the car information  
#using XML package
getdatavin.1 = function(node){
 vin = xmlAttrs(node)["data-vin"]
}

scrapeInfo.1 <- function(url)
{
  doc = htmlParse(url)
  temp = getNodeSet(doc, "//a[@data-vin]")
  vins = unique(sapply(temp,getdatavin.1))
  make = xpathSApply(doc,"//span[@itemprop='manufacturer']",xmlValue)
  model = xpathSApply(doc,"//span[@itemprop='model']",xmlValue)
  trim = xpathSApply(doc,"//span[@itemprop='trim']",xmlValue)
  year = xpathSApply(doc,"//span[@ itemprop='releaseDate']",xmlValue)
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
} 


#scrape car information from all the pages
alldata.1 = function(url){
  links = getLinklist.1(url)
  tt = lapply(links, scrapeInfo.1)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}
#links = getLinklist.1(url)
#tt = lapply(links, scrapeInfo.1)
#cardata = alldata.1(url)

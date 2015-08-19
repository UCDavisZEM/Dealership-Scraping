#url = "http://www.warehamfordinc.com/new-inventory?vehicle_type=All"


#http://www.warehamfordinc.com/new-inventory/?vehicle_type=All&offset=20&limit=20
#
getLinklist.17 = function(url){
  doc = htmlParse(url)
  baselink = url
    #total number of cars in new inventory
    totalnumber = as.numeric(xmlAttrs(getNodeSet(doc, "//input[@id='total_count']")[[1]])['value'])
    #each pages start number
    startnumber = seq(0, totalnumber-1, 20)
    #all the links 
    Linklist = sapply(1:length(startnumber), function(i) paste0(baselink, "&offset=", startnumber[i], "&limit=20"))
    return(Linklist)
}




#scrapping data
getdatacontent.17 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}


scrapeInfo.17 <- function(url)
{
  doc = htmlParse(url)
  
  vins = xpathSApply(doc, "//dd[@itemprop='serialNumber']",xmlValue)
  name.node =getNodeSet(doc, "//li/input[@id='nameplate' and @value]")
  name = sapply(name.node,getdatacontent.17, content = "value")
  tt = strsplit(name, " ")
  make = unname(sapply(tt, "[", 1))
  model = unname(sapply(tt, "[", 2))
  trim = "NA"
  year = as.numeric(xpathSApply(doc, "//time[@itemprop='releaseDate']",xmlValue))
  df <- data.frame(vins,make,model,trim, year, stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
} 

#scrape car information from all the pages
alldata.17 = function(url){
  require(XML)
  require(plyr)
  links = getLinklist.17(url)
  cardata = ldply(links, scrapeInfo.17)
  return(cardata)
}

##testdata = alldata.17(url)
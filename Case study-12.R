##Case Study 12
#Pattern inventory/view/New/
#test case
#url = "http://sullivanbrotherstoyota.com/inventory/view/New/"
#url = "http://toyotaofdartmouth.com/inventory/view/New/"
#url = "http://baystateford.com/inventory/view/New/"
#url = "http://www.tasca.com/Inventory/view/Make/Ford/New/"
getLinklist.12 = function(url){
  
  doc = htmlParse(url)
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  
 TotalVehicle  = as.numeric(strsplit(
   xpathSApply(doc, "//div[@class='recordCount']",xmlValue), " ")[[1]][1])
  RecordsPerPage = as.numeric(xpathSApply(
    doc, "//div[@class='recordsperpage']/select/option[@selected]",xmlValue)[1])
  TotalPage = floor(TotalVehicle/RecordsPerPage)+1
  
 href = unique(getHTMLLinks(url))
 index = grep("Page[0-9]+", href)
 pagelink = href[index][1]
  
 Linklist = sapply(1:TotalPage, 
                           function(i) paste0(baselink, 
                                              gsub("Page([0-9]+)", paste0("Page", i), pagelink)))  
  
  return(Linklist)
}

getdatacontent.12 = function(node){
  xmlAttrs(node)["content"]
}

scrapeInfo.12 <- function(url)
{
  doc = htmlParse(url)
  
  vin.node = getNodeSet(doc, "//meta[@itemprop='productID']")
  vins = unique(sapply(vin.node,getdatacontent.12))
  
  make.node = getNodeSet(doc, "//meta[@itemprop='brand']")
  make = sapply(make.node,getdatacontent.12)
  
  model.node = getNodeSet(doc, "//meta[@itemprop='model']")
  model = sapply(model.node,getdatacontent.12)
  
  year.node = getNodeSet(doc,"//meta[@itemprop='releaseDate']")
  year = sapply(year.node,getdatacontent.12)
  
  trim = xpathSApply(doc,"//span[@class='trim']",xmlValue)
  
  if(length(vins)==0){
    return(NULL)
  }
  else{
    df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
    colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
    return(df)
  } 
} 

#scrape car information from all the pages
alldata.12 = function(url){
  require(plyr)
  links = getLinklist.12(url)
  cardata = ldply(links, scrapeInfo.12)
  return(cardata)
}
#url = "http://sullivanbrotherstoyota.com/inventory/view/New/"
#url = "http://toyotaofdartmouth.com/inventory/view/New/"
#url = "http://baystateford.com/inventory/view/New/"
#url = "http://www.tasca.com/Inventory/view/Make/Ford/New/"
#tt = alldata.12(url)

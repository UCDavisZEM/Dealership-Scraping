url = "http://www.vacavillehonda.com/search/new/tp/"
#doc = htmlParse(url)
#baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)



#grab the linklist

#small function to get page links
getPagelink = function(lastpagelink, pagenumber){
  lastpagenumber = as.numeric(substr(lastpage, gregexpr("p:", lastpage)[[1]][2]+2,
                                     gregexpr("p:", lastpage)[[1]][2]+2))
  pagelink = paste0(strsplit(lastpage, lastpagenumber)[[1]][1], pagenumber, 
                    strsplit(lastpage, lastpagenumber)[[1]][2])
  return(pagelink)
}


getLinklist.2 = function(url){
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  href = unique(getHTMLLinks(url))
  index = gregexpr("/",url)[[1]]
  pattern = substr(url, index[3],nchar(url) )
  temp = href[grep(pattern,href)]
  
  ind = grep("p:", gsub(pattern, "", temp))
  #get last page's link
  lastpage = paste0(baselink, temp[ind[length(ind)]])
  #get the number of total pages from the last page link
  lastpagenumber = as.numeric(substr(lastpage, gregexpr("p:", lastpage)[[1]][2]+2,
                                     gregexpr("p:", lastpage)[[1]][2]+2))
  
  Linklist = sapply(1:lastpagenumber, getPagelink, lastpagelink = lastpage)
  
  return(Linklist)
}






#scrapping data
getdatacontent = function(node){
  vin = xmlAttrs(node)["content"]
}


scrapeInfo.2 <- function(url)
{
  doc = htmlParse(url)
  
  vin.node = getNodeSet(doc, "//meta[@itemprop='serialNumber']")
  vins = unique(sapply(vin.node,getdatacontent))
  
  make.node = getNodeSet(doc, "//meta[@itemprop='manufacturer']")
  make = sapply(make.node,getdatacontent)
  
  model.node = getNodeSet(doc, "//meta[@itemprop='model']")
  model = sapply(model.node,getdatacontent)
  
  year.node = getNodeSet(doc,"//meta[@itemprop='releaseDate']")
  year = sapply(year.node,getdatacontent)
  
  trim.node = getNodeSet(doc, "//meta[@itemprop='name']")[-1]
  name = sapply(trim.node,getdatacontent)
  trim = sapply(1:length(name), 
                function(i){gsub("^\\s+|\\s+$", "", 
                        strsplit(name[i], model[i])$content[2])})
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
} 

#scrape car information from all the pages
alldata.2 = function(url){
  links = getLinklist.2(url)
  tt = lapply(links, scrapeInfo.2)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}



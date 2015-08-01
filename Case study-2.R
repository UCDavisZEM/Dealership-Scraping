#url = "http://www.vacavillehonda.com/search/new/tp/"
#doc = htmlParse(url)
#baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)

#url = "http://www.harr.com/search/new/tp/"

#grab the linklist

#small function to get page links
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
  lastpagenumber = as.numeric(gsub(".*p:([0-9]+).*", "\\1", lastpage))
  Linklist = sapply(1:lastpagenumber, function(i)gsub("p:([0-9]+)", paste0("p:", i), lastpage))
  
  return(Linklist)
}






#scrapping data
getdatacontent.2 = function(node){
 xmlAttrs(node)["content"]
}


scrapeInfo.2 <- function(url)
{
  doc = htmlParse(url)
  
  vin.node = getNodeSet(doc, "//meta[@itemprop='serialNumber']")
  vins = unique(sapply(vin.node,getdatacontent.2))
  if(length(vins)==0){
    return(NULL)
  }
  else{
  make.node = getNodeSet(doc, "//meta[@itemprop='manufacturer']")
  make = sapply(make.node,getdatacontent.2)
  
  model.node = getNodeSet(doc, "//meta[@itemprop='model']")
  model = sapply(model.node,getdatacontent.2)
  
  year.node = getNodeSet(doc,"//meta[@itemprop='releaseDate']")
  year = sapply(year.node,getdatacontent.2)
  
  trim.node = getNodeSet(doc, "//meta[@itemprop='name']")[-1]
  name = sapply(trim.node,getdatacontent.2)
  trim = sapply(1:length(name), 
                function(i){gsub("^\\s+|\\s+$", "", 
                        strsplit(name[i], model[i])$content[2])})
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  print(url)
  return(df)
  }
} 

#scrape car information from all the pages
alldata.2 = function(url){
  links = getLinklist.2(url)
  tt = lapply(links, scrapeInfo.2)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#cc = alldata.2(url)

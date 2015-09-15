##Case 23
##pattern: /vehicle/search/new/?page=
url = "http://www.newcenturybmw.com/vehicle/search/new/"


#grab the linklist

getLinklist.23 = function(url){
  doc = htmlParse(url)
  totalnumber = as.numeric(unique(xpathSApply(doc, "//span[@class='results-stats-ttl']", xmlValue)))
  totalpage = length(seq(1, totalnumber-1, 25))
  Linklist = paste0(url, "?page=", 1:totalpage)
  return(Linklist)
}

getdatacontent.23 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

scrapeInfo.23 <- function(url)
{
  doc = htmlParse(url)
  nodes = getNodeSet(doc, "//div[@data-vinfo]")
  vinfo = unique(sapply(nodes,getdatacontent.23, content = "data-vinfo"))
  vinfo.temp   = sapply(vinfo, fromJSON, simplifyVector = T, simplifyDataFrame = T)
  vins = unlist(unname(vinfo.temp[1, 1:dim(vinfo.temp)[2]]))
  make = unlist(unname(vinfo.temp[2, 1:dim(vinfo.temp)[2]]))
  model = "NA"
  trim = "NA"
  year = unlist(unname(vinfo.temp[4, 1:dim(vinfo.temp)[2]]))
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
} 

#scrape car information from all the pages
alldata.23 = function(url){
  links = getLinklist.23(url)
  tt = lapply(links, scrapeInfo.23)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#testdata = alldata.23(url)

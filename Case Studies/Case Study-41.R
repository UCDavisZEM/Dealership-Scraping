
#case study 41
#pattern: com/search/New+t
#url = "http://www.michaeltoyota.com/search/New+t"
#url = "http://www.toyotaofelcajon.com/search/New+t"





getLinklist.41 = function(url){
  baselink = url
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  totalcars = as.numeric(gsub("New \\(([0-9]+)\\)", "\\1", xpathSApply(doc, "//option[@value='New']", xmlValue)[2]))
  totalpage = length(seq(0, totalcars-1, 10))
  Linklist = sapply(1:totalpage, function(i) paste0(baselink, "?page=", i))         
  return(Linklist)
}

getdatacontent.41 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

scrapeInfo.41 <- function(url)
{
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  nodes = getNodeSet(doc, "//input[@name='vin']")
  vins = unique(sapply(nodes,getdatacontent.41, content = "value"))
  make.nodes = getNodeSet(doc, "//input[@name='make']")
  
  make = sapply(make.nodes,getdatacontent.41, content = "value")
  model = "NA"
  trim = "NA"
  year = NA
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
} 

#scrape car information from all the pages
alldata.41 = function(url){
  links = getLinklist.41(url)
  tt = lapply(links, scrapeInfo.41)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#testdata = alldata.41(url)

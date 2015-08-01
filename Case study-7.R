#library(RCurl)
#library(XML)
#test cases 
#url = "http://www.lundgrenhondaofgreenfield.com/inventory/newsearch/New/"
#url = "http://www.hondacarsofboston.com/inventory/newsearch/New/"


getdatacontent.7 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getLinklist.7 = function(url){
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  doc = htmlParse(url)
  
  lastlink = xmlAttrs(getNodeSet(doc, "//li[@class='next']/a[@href]")[[2]])["href"]
  
  totalpage = as.numeric(gsub(".*Page([0-9]+).*", "\\1", lastlink))
  
  Linklist = sapply(1:totalpage, 
                    function(i) 
                      paste0(baselink, 
                            gsub("Page([0-9]+)", paste0("Page", i), lastlink)))  
  
  return(Linklist)
}




scrapeInfo.7 <- function(url)
{
  doc = htmlParse(url)
  #print(url)
  nodes = getNodeSet(doc, 
                     "//img[@alt and @id and not(contains(@class, 'vehiclestyleimg'))]")
  if(length(nodes)==0){
    return(NULL)
  }
  else{
    temp = unname(sapply(nodes, getdatacontent.7, content = "alt"))
    vins = sapply(strsplit(temp, " "), tail, n = 1 )
    ##or nodes = getNodeSet(doc, "//img[@alt]");temp = unname(sapply(nodes, getdatacontent.7, content = "alt"))
    ##, vins = gsub("(.*)([0-9A-Z]{17})$", "\\2", temp)[grepl("^([0-9A-Z]{17})$", gsub("(.*)([0-9A-Z]{17})$", "\\2", temp))] 
    make = xpathSApply(doc,"//span[@class='make']",xmlValue)
    model = xpathSApply(doc,"//span[@class='model']",xmlValue)
    trim = xpathSApply(doc,"//span[@class='trim']",xmlValue)
    year = xpathSApply(doc,"//span[@class='year']",xmlValue)
    
    df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
    colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
    #print(url)
    return(df) 
  }
  
} 

#scrape car information from all the pages
alldata.7 = function(url){
  
  links = getLinklist.7(url)
  tt = lapply(links, scrapeInfo.7)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#testdata = alldata.7(url)



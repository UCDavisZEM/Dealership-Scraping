#library(XML)
#url = "http://www.hyannishonda.com/Southeastern-Massachusetts/For-Sale/New/"

#xpathSApply( doc, "//section[@class='results-count']",xmlValue)
#class="nav-arrow last"
getdatacontent.10 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}



getLinklist.10 = function(url){
  
  doc = htmlParse(url)
  baselink = url
  totalcars = as.numeric(strsplit(
    xpathSApply(doc, "//section[@class='results-count']",xmlValue), " ")[[1]][1])
  
  totalpage = length(seq(1,totalcars, 20))
  
  Linklist = unname(sapply(1:totalpage, 
                    function(i) 
                      paste0(baselink, paste0("?Page=", i), "&Pagesize=20")))  
  
  return(Linklist)
}

scrapeInfo.10 <- function(url)
{
  doc = htmlParse(url)
  
  vin.node = getNodeSet(doc,"//input[@class='vehicle-vin']")
  if(length(vin.node)!=0){
    vins = sapply(vin.node,getdatacontent.10, content = "value")
    
    make.node = getNodeSet(doc,"//input[@class='vehicle-make']")
    make = sapply(make.node,getdatacontent.10, content = "value")
    
    model.node = getNodeSet(doc,"//input[@class='vehicle-model']")
    model = sapply(model.node,getdatacontent.10, content = "value")
    
    trim.node = getNodeSet(doc,"//input[@class='vehicle-body']")
    trim = sapply(trim.node,getdatacontent.10, content = "value")
    
    year.node = getNodeSet(doc,"//input[@class='vehicle-year']")
    year = sapply(year.node,getdatacontent.10, content = "value") 
  }
  else{
    nodes = getNodeSet(doc, "//li[@data-incentives]")
    templist = lapply(nodes,getdatacontent.10, content = "data-incentives")
    ttt = sapply(1:length(templist), 
                                 function(i) 
                                   fromJSON(templist[[i]], simplifyVector = T, simplifyDataFrame = TRUE))
    ttt = as.data.frame(t(ttt))
    
    vins = as.character(unlist(ttt$vin))
    name = xpathSApply(doc,"//a[@class='vehicle-info']",xmlValue, trim = TRUE)
    tt = strsplit(name, " ")
    make = sapply(tt, "[", 2)
    model = "NA"
    trim = "NA"
    year = sapply(tt, "[", 1)
  }
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
} 

#scrape car information from all the pages
alldata.10 = function(url){
  links = getLinklist.10(url)
  tt = lapply(links, scrapeInfo.10)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#url = "http://www.hyannishonda.com/Southeastern-Massachusetts/For-Sale/New/"
#url = "http://imperialfordmilford.com/Providence-RI-Worcester-MA/For-Sale/New/"

#testdata = alldata.10(url)



#Case Study 31
#pattern: inventory?type=new
#url = "http://www.fairfieldbmw.com/inventory?type=new"
#url = "http://www.tameronhondagadsden.com/inventory?type=new"
#url = "http://www.hondaofmurfreesboro.com/inventory?type=new"
#small function to get page links
getLinklist.31 = function(url){
  baselink = substr(url, 1, gregexpr("/",url)[[1]][3]-1)
  href = unique(getHTMLLinks(url))
  
  ind = grep("pg=", href)
  #get last page's link
  if(!grepl(pattern = "http", href[ind][1])){
    lastpage = paste0(baselink, href[ind[length(ind)]])
  }
  else{
    lastepage = href[ind[length(ind)]]
  }
  #get the number of total pages from the last page link
  lastpagenumber = as.numeric(gsub(".*pg=([0-9]+).*", "\\1", lastpage))
  Linklist = sapply(1:lastpagenumber, function(i)gsub("pg=([0-9]+)", paste0("pg=", i), lastpage))
  
  return(Linklist)
}


#scrapping data
getdatacontent.31 = function(node){
  xmlAttrs(node)["content"]
}


scrapeInfo.31 <- function(url)
{
  doc = htmlParse(url)
  
  vin.node = getNodeSet(doc, "//meta[@itemprop='serialNumber']")
  vins = unique(sapply(vin.node,getdatacontent.2))
  if(length(vins)==0){
    return(NULL)
  }
  else{
    make.node = getNodeSet(doc, "//meta[@itemprop='manufacturer']")
    make = sapply(make.node,getdatacontent.31)
    
    
    model = "NA"
    year = NA
    trim = "NA"
    
    df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
    colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
    #print(url)
    return(df)
  }
} 

#scrape car information from all the pages
alldata.31 = function(url){
  links = getLinklist.31(url)
  tt = lapply(links, scrapeInfo.31)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#cc = alldata.31(url)

#Case study 36
#url = "http://www.bmwofchampaign.com/search/s:yr1/?q=&tp=new&mk=8-bmw/"

getLinklist.36 = function(url){
  doc = htmlParse(url)
  baselink = gsub('(.*com).*','\\1',url)
  countNode = xmlValue(getNodeSet(doc,"//div[@class='srp_results_count_container']/span/text()")[[1]],trim=T)
  count = as.numeric(gsub("([0-9]+) .*","\\1",countNode))
  
  numNode = xmlValue(getNodeSet(doc,"//select[@class='sort_results_box']/option[@selected='selected']/text()")[[1]],trim=T)
  num = as.numeric(numNode)
  
  pages = ceiling(count/num)
  links = unname(sapply(1:pages, function(i) paste0(baselink,'/search/new-bmw/tp-mk8/s:yr1,p:',i,'/')))
  return(links)
}

scrapeInfo.36 =function(url){
  txt = getURLContent(url, useragent = "R")
  doc = htmlParse(txt, asText = TRUE)
  
  vin.node = getNodeSet(doc, "//meta[@itemprop='serialNumber']")
  vins = unique(sapply(vin.node,getdatacontent.2))
  if(length(vins)==0){
    return(NULL)
  }
  else{
    make.node = getNodeSet(doc, "//meta[@itemprop='manufacturer']")
    make = sapply(make.node,getdatacontent.2)
    
    
    model = "NA"
    year = NA
    trim = "NA"
    
    df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
    colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
    #print(url)
    return(df)
  }
}

alldata.36 = function(url){
  require(XML)
  require(plyr)
  links = getLinklist.36(url)
  cardata = ldply(links, scrapeInfo.36)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(cardata)
}
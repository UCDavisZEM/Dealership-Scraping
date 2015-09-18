##Case 27
#special case for 
#url = "http://shopautobahn-bmw.ebizautos.com/plugin-inventory.aspx?_new=true"

getdatacontent.27 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getLinklist.27 = function(url){
  
    baselink = url
    txt = getURLContent(url, useragent = "R")
    doc = htmlParse(txt, asText = TRUE)
    totalpage = max(as.numeric(unique(xpathSApply(doc, "//div[@class='PageBox']", xmlValue))))
    Linklist = paste0(baselink, "&_page=", 1:totalpage)
    return(Linklist)
 
}

getCarlink.27 = function(url){
  doc = htmlParse(url)
  link.node = getNodeSet(doc, "//a[@class='fBold title fs20']")
  link = sapply(link.node, getdatacontent.27, content = "href")
  base = substr(url, 1, gregexpr("/",url)[[1]][3])
  links = paste0(base, link)
  return(links)
}

scrapeInfo.27 = function(url){
  doc = htmlParse(url)
  vin.node = getNodeSet(doc, "//div[@class='mgb5']")
  vins = unique(gsub(".*([0-9A-z]{17}).*","\\1",xmlApply(vin.node, xmlValue)))
  vins = vins[grep("[0-9]", vins)]
  make = "BMW"
  model = "NA"
  trim = "NA"
  year = NA
  
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  #print(url)
  return(df)
}

alldata.27 = function(url){
 pagelinks = getLinklist.27(url)
carlinks = unlist(lapply(pagelinks, getCarlink.27))
  tt = lapply(carlinks, scrapeInfo.27)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

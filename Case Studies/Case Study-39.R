#special
#case study 39
#url = "http://www.orrbmw.com/inventory/"



getdatacontent.39 = function(node, content){
  tt = xmlAttrs(node)[content]
  return(tt)
}
getLinklist.39 = function(url){
  nurl = "http://www.orrbmw.com/inventory/?vType=2&style=0&Model=0&Transmission=0&ptm1=0&sb=11&yf=1980&yt=2016&pf=0&pt=any"
  doc = htmlParse(nurl)
  Linknode = getNodeSet(doc, "//li[@class='MarkupPagerNavLastNum']/a")
  lastlink = xmlAttrs(Linknode[[1]])["href"]
  totalnumber = as.numeric(gsub(".*page([0-9]+)\\?.*", "\\1", lastlink))
  links = sapply(2:totalnumber, function(x)paste0("http://www.orrbmw.com", 
                                                  gsub("page[0-9]+", paste0("page", x), lastlink)))
  Linklist = c(nurl, links)
  return(Linklist)
  
}

scrapeInfo.39 = function(url){
  doc = htmlParse(url)
  nodes = getNodeSet(doc, "//div[@class='InvTitle']/a")
  tempurl = toupper(sapply(nodes, getdatacontent.39, content='href'))
  vins = unique(gsub(".*\\/([0-9A-z]{17}).*","\\1",tempurl))
  name = unlist(xmlApply(nodes, xmlValue))
  make = sapply(strsplit(name, " "), "[", 2)
  
  model = "NA"
  trim = "NA"
  year = NA
  df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(df)
}

alldata.39 = function(url){
  links = getLinklist.39(url)
  tt = lapply(links, scrapeInfo.39)
  cardata = Reduce(function(x, y) rbind(x, y), tt)
  return(cardata)
}

#ttt = alldata.39(url)

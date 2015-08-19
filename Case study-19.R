#Case 19 (this case needs to go deeper to get vins)
#url = "http://www.stonehamford.com/new-ford-inventory-boston.aspx"
#url = "http://www.thomasford.com/inventory.aspx"  

alldata.19 = function(url){
  require(XML)
  require(plyr)
  links = getallLinklist.19(url)
  cardata = ldply(links, scrapeInfo.19)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(cardata)
}

getallLinklist.19 = function(url){
  doc = htmlParse(url)
  countNode = xmlValue(getNodeSet(doc,'//div[@class="floatLeft fBold numVehiclesFoundNumber"]/text()')[[1]])
  startindex = gregexpr('[0-9]+',countNode)[[1]]
  numlen = attr(gregexpr('[0-9]+',countNode)[[1]],which = 'match.length')
  totalnum = as.numeric(substr(countNode,startindex,(startindex+numlen-1)))
  pagenum = ceiling(totalnum/10)
  
  #get all page links
  baselink = paste0(url, "?_page=1")
  linklist = sapply(1:pagenum,function(i) gsub("([0-9]+)", toString(i), baselink))
  #get all car links
  allcarlinks = unlist(lapply(linklist,getcarlinks.19))
  
  return(allcarlinks)
}

getcarlinks.19<- function(url)
{
   #print(url)
   doc = htmlParse(url)
   links = getHTMLLinks(doc)
   links = unique(links[grep(pattern = '/detail-',links)])  
   return(links)
}

scrapeInfo.19<-function(url)
{
  #print(url)
  #url = "http://www.stonehamford.com/detail-2014-ford-transit_connect_wagon-4dr_wgn_swb_xlt-new-11767013.html"
  doc = htmlParse(url)
  vinNode = getNodeSet(doc,'//div[@class="mgb5"]')
  vins = xmlValue(xmlChildren(x = vinNode[[1]])[[3]],trim=T)
  contentNode = xmlValue(xpathSApply(doc, '//div[@class="middleModelAndDesc"]/text()')[[1]],trim=T)
  year = gsub('([0-9]+).*','\\1',contentNode)
  make = gsub('.*\t(.*)\r.*','\\1',contentNode)
  model = "NA"
  trim = "NA"
  
  return(data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F))
}
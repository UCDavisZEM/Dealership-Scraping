#Case study 21
#url = "http://www.balisetoyota.com/web/inventory/new"

alldata.21 = function(url){
  require(XML)
  require(plyr)
  links = getallLinklist.21(url)
  cardata = ldply(links, scrapeInfo.21)
  colnames(cardata) <- c("VIN", "Make", "Model", "Trim", "Year")
  return(cardata)
}

#http://www.balisetoyota.com/web/inventory/All_years/All_makes/All_models/All_body_types/new/?sort_by=makefirst&start=0
getallLinklist.21 = function(url){
  doc = htmlParse(url)
  countNode = xmlValue(getNodeSet(doc,'//h3[@class="non_expandable_menuheader"]/text()')[[1]])
  startindex = gregexpr('[0-9]+',countNode)[[1]]
  numlen = attr(gregexpr('[0-9]+',countNode)[[1]],which = 'match.length')
  totalnum = as.numeric(substr(countNode,startindex,(startindex+numlen-1)))
  
  #get all page links
  baselink = paste0(url, "?start=0")
  linklist = sapply(seq(from = 0,to = totalnum-1,by = 50),function(i) gsub("([0-9]+)", toString(i), baselink))
  #get all car links
  allcarlinks = unlist(lapply(linklist,getcarlinks.21))
  allcarlinks = unname(sapply(allcarlinks, function(str) paste0(strsplit(url,split = '/web')[[1]][1],str)))
  return(allcarlinks)
}

getcarlinks.21<- function(url)
{
  #print(url)
  doc = htmlParse(url)
  links = getHTMLLinks(doc)
  links = unique(links[grep(pattern = '/web/new.*[0-9]$',links)])  
  return(links)
}

scrapeInfo.21<-function(url)
{
  #print(url)
  #url = "http://www.balisetoyota.com/web/new/Toyota-4Runner-2015-West-Springfield-Massachusetts/20934167/?condition_id=10425"
  doc = htmlParse(url)
  vinNode = getNodeSet(doc,'//li[contains(.,"VIN")]/text()')
  vin = xmlValue(vinNode[[1]],trim=T)
  contentNode = xmlValue(xpathSApply(doc, '//h1[@id="vehicle_title"]/text()')[[1]],trim=T)
  year = gsub('([0-9]+).*','\\1',contentNode)
  make = gsub('[0-9]{4} (.*?) .*','\\1',contentNode)
  model = "NA"
  trim = "NA"
  
  return(data.frame(vin,make,model,trim,as.numeric(year), stringsAsFactors = F))
}
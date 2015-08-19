###SOLVED
#Special case--
#url = "http://www.quirkchevy.com/new-vehicles/"
#url ="http://www.hondanorth.com/new-vehicles/"
#Not solved
#url = "http://www.silkohonda.com/new-vehicles/" 

alldata.5 = function(url){
  require(XML)
  doc = htmlParse(url)
  txt = xpathSApply(doc, "//script[@type='text/javascript']",xmlValue)  
  txt = txt[grep("inventory_localization",txt)]
  if(length(txt)>0)
  {
    vin_str = gsub('.*vin_numbers.*\\[(.*?)\\].*','\\1',txt)
    vin_str = gsub('\"','', unlist(strsplit(vin_str,',')))
    
    make_str = gsub('.*vehicle_makes.*\\[(.*?)\\].*','\\1',txt)
    make_str = gsub('\"','', unlist(strsplit(make_str,',')))
    
    models_str = gsub('.*models.*\\[(.*?)\\].*','\\1',txt)
    models_str = gsub('\"','', unlist(strsplit(models_str,',')))
    
    year = rep(NA, length(vin_str))
    trim = rep(NA, length(vin_str))
    df <- data.frame(vin_str,make_str,models_str,trim, year,stringsAsFactors = F)
    colnames(df) <- c("VIN", "Make", "Model","Trim", "Year")
  }
  else
  {
    require(plyr)
    require(RSelenium)
    links = getLinklist.5(url,doc)
    
    RSelenium::startServer()
    remDr = remoteDriver(browserName = "firefox")
    remDr$open(silent = TRUE)
    tt = list()
    for(url in links)
    {
      print(url)
      remDr$navigate(url)    
      Sys.sleep(4)
      txt=remDr$getPageSource()
      tt[[url]] = scrapeInfo.5(txt[[1]])
    }
    df = Reduce(function(x, y) rbind(x, y), tt)
    colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
    remDr$close
  }   
  return(df)
}

getLinklist.5 = function(url,doc){
  baselink = url
  #total number of cars in new inventory
  countNode = xmlValue(getNodeSet(doc, "//div[@class='resultsCount']/h4/text()")[[1]])
  totalnumber = as.numeric(gsub('([0-9]+).*','\\1',countNode))
  #get how many pages
  pagenumber = ceiling(totalnumber/20)
  #all the links 
  Linklist = sapply(1:pagenumber, function(i) paste0(baselink, "#action=im_ajax_call&perform=get_results&paged=", i, "&limit=20"))
  return(Linklist)
}

scrapeInfo.5 <- function(doc)
{
    doc = htmlParse(doc)
    vins = unique(xpathSApply(doc, "//div[@class='vehicle-overview']", xmlGetAttr,name='id'))
    num = length(xpathSApply(doc, "//div[@class='vehicle-overview']", xmlGetAttr,name='title'))
    names = xpathSApply(doc, "//div[@class='vehicle-overview']", xmlGetAttr,name='title')[seq(1,num,2)]
    tt = strsplit(names, " ")    
    make = sapply(tt, "[", 2)
    model = "NA"
    trim = "NA"
    year = sapply(tt, "[", 1)
    
    return(data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F))
}
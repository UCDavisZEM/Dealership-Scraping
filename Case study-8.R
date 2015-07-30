#library(XML)
##the only input url
#test case
# url  = "http://westborotoyota.com/inventory/condition=new/"

#library(RSelenium)
#scrap all the car information  
#XPath: //*[@id="vehicle-list"]/li[1]/section/ul/li[6]/a
#CSS :#vehicle-list > li > section > ul > li > a
#weild here : only returns the first result ...  
#linkInfo <- remDr$findElement(using = "link text", "See Details *")
#length(linkInfo) 
#linkInfo$highlightElement
#linkInfo$getElementAttribute('href')

#Using phantomJS
#system("upx -d phantomjs-2.0.0-macosx/bin/phantomjs")
getLinkList.8 <- function(url)
{
    require(RSelenium)
    #pJS <- phantom(pjs_cmd = "./phantomjs-2.0.0-macosx/bin/phantomjs")
    #Sys.sleep(5)
    #remDr <- remoteDriver(browserName = 'phantomjs')
    remDr = remoteDriver(browserName = "firefox")
    remDr$open(silent = TRUE)
    remDr$navigate(url)
    
    #this following approach works
    doc = remDr$getPageSource()
    tdoc = htmlParse(doc[[1]])
    links = getHTMLLinks(tdoc) 
    vlinks = unique(links[grep('/vehicle/[0-9A-Z]{17}-',links)])
    #length(vlinks)
    remDr$close
    #pJS$stop()
    return(vlinks)
}
#//div[@class='inveContainer']//ul/li[@class='vinDisplay']
#linkInfo <- remDr$findElement(using = "xpath", "//div[@class=vehiclePhoto]")
#successfully get the node set


scrapeInfo.8 <- function(links)
{
  vins = unique(gsub(".*([0-9A-Z]{17}).*",'\\1',links))
  make = gsub(".*-[0-9]{4}-(.*?)-.*",'\\1',links)
  model = gsub(".*-[0-9]{4}-.*-(.*?)-.*",'\\1',links)
  trim = rep(NA,length(links))
  year = gsub(".*([0-9]{4}).*",'\\1',links)
  df <- data.frame(vins,make,model,trim, year)
  colnames(df) <- c("VIN", "Make", "Model","Trim", "Year")
  return(df)
} 


#scrape car information from all the pages
alldata.8 = function(url){
  vlinks = getLinkList.8(url)
  cardata = scrapeInfo.8(vlinks)
  return(cardata)
}
#links = getLinklist.1(url)
#tt = lapply(links, scrapeInfo.1)
#cardata = Reduce(function(x, y) rbind(x, y), tt)

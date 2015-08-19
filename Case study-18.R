url = "http://www.milfordnissan.com/new-nissans/new-vehicle-inventory.html?reset=1"
url = "http://www.woburntoyota.com/new/new-vehicle-inventory.html?reset=1"



alldata.18 = function(url){
  require(RSelenium)
  require(XML)
  require(RCurl)
  RSelenium::startServer()
  checkForServer()
  cprof <- getChromeProfile(dataDir = "~/Library/Application Support/Google/Chrome","~/Library/Application Support/Google/Chrome/Default")
  remDr = remoteDriver(browserName = "firefox")
  remDr$open(silent = TRUE)
  remDr$navigate(url)  
  
  for(i in 1:10){
    remDr$executeScript(paste("scroll(0,",i*100000,");"))
  }
  
  webElem<-remDr$findElement(using = 'xpath', "//div/a[@class='scroll-for-more-bar']")
  url = unlist(webElem$getElementAttribute('href'))
  remDr$navigate(url)
  newurl
  remDr$buttondown(buttonId=0)
  #//*[@id="block-2"]/div[2]/div[2]/div/a
  #webElem$getElementText()
  #length(webElem)
  while(length(remDr$findElement(using = 'xpath', "//div/a[@class='scroll-for-more-bar']")>0))
  {
    
  }
}

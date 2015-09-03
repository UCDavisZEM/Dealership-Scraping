###NISSAN
install.packages("XML", repos = "http://cran.cnr.Berkeley.edu/")
library(XML)
install.packages("RCurl", repos = "http://cran.cnr.Berkeley.edu/")
library(RCurl)

url = "http://www.nissanusa.com/nissandealers/location/"
doc = htmlParse(url)
statenodes = getNodeSet(doc, "//div[@id='choose_state_bottom']/ul/li/a[@href]")

getcontent= function(node, content = "href"){
  tt = xmlAttrs(node)[content]
  return(tt)
}

statelink = unname(sapply(statenodes, getcontent, content = "href"))

getstatedealer = function(url){
  doc = htmlParse(url)
  citynodes = getNodeSet(doc, "//div[@class='dealer_city_section ']/a[@href]")
  links = unname(sapply(citynodes, getcontent, content = "href"))
  return(links)
}
citylinks = unname(sapply(statelink, getstatedealer))
citylinks = unlist(citylinks)

getdealer = function(cityurl){
  print(cityurl)
  if(url.exists(cityurl)){
    doc = htmlParse(cityurl)
    DealerName =xpathSApply(doc, "//div[@class='dealer_city_section_1']/h1",xmlValue)
    print(DealerName)
    if(length(DealerName)==0){return(NULL)}
    else{
      Address = paste(gsub("^\\s+|\\s+$", "",
                           xpathSApply(doc, "//div[@class='dealer_city_section_1']/p",xmlValue)[1:2]),
                      collapse = ", ")
      Link = unname(sapply(getNodeSet(doc, "//a[@class='seo_viewdealer_cta']"),getcontent,content='href'))
      Zipcode = tail(strsplit(Address, " ")[[1]], n = 1)
      df <- data.frame(DealerName, Address, Link, Zipcode, index = cityurl, stringsAsFactors = F)
      return(df)
    }
  }
  else{return(NULL)}
}

tempdata = lapply(citylinks, getdealer)
NissanDealer = Reduce(function(x, y) rbind(x, y), tempdata)
NissanDealer = NissanDealer[!duplicated(NissanDealer), ]

load("zipdata.rdata")
names(NissanDealer)[4]="zipcode"
NissanDealers = merge(NissanDealer, zipdata)
NissanDealers$Latitude = "NA"
NissanDealers$Longitude = "NA"
NissanDealers$IV_link = "NA"
save(NissanDealers, file = "nissanDealers.rdata")

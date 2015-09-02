#Collect all the nation-wide Ford dealerships with information about name, address, website, inventory link and geo location 
require(XML)
require(plyr)

mainurl = "http://content.dealerconnection.com/vfs/brands/us_ford_en.html"

baseurl = gsub("(.*/)us.*",'\\1',mainurl)

txt <- htmlParse(mainurl)
links <- getHTMLLinks(txt)
statelinks <- unname(sapply(links, function(link) paste0(baseurl,link)))

#url = "http://content.dealerconnection.com/vfs/brands/us/ca_ford_en.html"
getcitylinks <- function(url){
  citybaseurl = gsub('(.*us/).*','\\1',url)
  doc <- htmlParse(url)
  links <- getHTMLLinks(doc)[-1]
  citylinks <- unname(sapply(links, function(link) paste0(citybaseurl,link)))
  return(citylinks)
}

#url = "http://content.dealerconnection.com/vfs/brands/us/ca/san_francisco_ford_en.html"
getdealerinfor <- function(url){
  print(url)
  doc <- htmlParse(url)
  links <- unique(getHTMLLinks(doc)[-c(1:2)])
  #dealerlink <- links[grepl(pattern = 'com$',links)]
  
  dealerNameNodes = getNodeSet(doc,'//div[@class="dealerName"]/b/a/text()')
  dealerName = xmlSApply(dealerNameNodes,xmlValue,trim=T)
  
  AddressNodes_road = getNodeSet(doc,'//div[@class="dealerAddress"]/label[1]/text()')
  dealer_road = xmlSApply(AddressNodes_road,xmlValue,trim=T)
  AddressNodes_city = getNodeSet(doc,'//div[@class="dealerAddress"]/label[2]/text()')
  dealer_city = xmlSApply(AddressNodes_city,xmlValue,trim=T)
  AddressNodes_state = getNodeSet(doc,'//div[@class="dealerAddress"]/label[4]/text()')
  dealer_state = xmlSApply(AddressNodes_state,xmlValue,trim=T)
  AddressNodes_zipcode = getNodeSet(doc,'//div[@class="dealerAddress"]/label[5]/text()')
  dealer_zipcode = xmlSApply(AddressNodes_zipcode,xmlValue,trim=T)
  
  dealerAddress = paste0(dealer_road,', ',dealer_city,', ',dealer_state,' ',dealer_zipcode)
  
  DealerWebsiteNodes = getNodeSet(doc, '//ul[@class="dealerListingListing dealerCities"]/li[1]/a')
  dealerWebsite =   xmlSApply(DealerWebsiteNodes,xmlGetAttr,"href")
  
  DealerWebsiteINNodes= getNodeSet(doc, '//ul[@class="dealerListingListing dealerCities"]/li[2]/a')
  dealerInventoryLink =  xmlSApply(DealerWebsiteINNodes,xmlGetAttr,"href")
  
  zipcode = gsub('.* ([0-9]{5}).*','\\1',dealerAddress)
  
  df <- data.frame(dealerName, dealerAddress, dealerWebsite,zipcode, dealerInventoryLink, stringsAsFactors=F )
  colnames(df) = c("Dealer","Address","Link","zipcode","IV_link" )
  return(df)
}

allcity_links = unlist(unname(sapply(statelinks,getcitylinks)))
fordDealer = ldply(allcity_links,getdealerinfor)
fordDealer = fordDealer[!duplicated(fordDealer$Address),]
#fordDealer = as.data.frame(fordDealer, stringsAsFactors=F)
#fordDealer[] <- lapply(fordDealer, as.character)

load("zipdata.rdata")
names(fordDealer)
fordDealers = merge(fordDealer, zipdata)
save(fordDealers, file="fordDealers.rdata")

#notmatch = setdiff(fordDealer$Dealer, fordDealers$Dealer)
#fordDealer$zipcode[fordDealer$Dealer %in% notmatch]
#fordDealers$city

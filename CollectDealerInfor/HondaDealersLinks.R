#HONDA DEALER
install.packages("XML", repos = "http://cran.cnr.Berkeley.edu/")
library(XML)
install.packages("RCurl", repos = "http://cran.cnr.Berkeley.edu/")
library(RCurl)

####get zipcode that may have honda dealer from dealer rater website
url = "http://www.dealerrater.com/directory/Massachusetts/Honda/"
doc = htmlParse(url)

statelist = gsub(" ", "-", xpathSApply(doc, "//select[@id='StateCode']/option",xmlValue)[-1])
statelinks = sapply(1:length(statelist), function(i)gsub("Massachusetts", statelist[i], url))

getHondastate = function(url){
  href = getHTMLLinks(url)
  nextpage = unique(href[grep("page=", href)])
  if(length(nextpage)!=0){
    nextlink = paste0("http://www.dealerrater.com", nextpage)
  }
  else{nextlink = NULL}
  links = c(url, nextlink)
  return(links)
}

state.all = sapply(statelinks, getHondastate)
stateurl = as.vector(unlist(unname(state.all)))
getall = function(url){
  a = unique(getHTMLLinks(url))
  index = grep("/dealer/", a)
  links = a[index]
  return(links)
}

temp  = lapply(stateurl, getall)
temp2 = unlist(temp)
temp3 = temp2[-grep("#", temp2)]

hondadealerlink = paste0("http://www.dealerrater.com", temp3)

getcontent= function(node, content = "href"){
  tt = xmlAttrs(node)[content]
  return(tt)
}

getDealer.honda = function(url){
  print(url)
  if(url.exists(url)){
    doc = htmlParse(url)
    DealerName = xpathSApply(doc, "//h1[@itemprop='name']", xmlValue)
    AddressNode = getNodeSet(doc, "//input[@id='end' and @type='hidden']")
    Address = unlist(unname(sapply(AddressNode, getcontent, content="value")))
    Zipcode = tail(strsplit(Address, " ")[[1]], n = 1)
    df <- data.frame(DealerName, Address, Zipcode, index = url, stringsAsFactors = F)
    return(df)
  }
  else{return(NULL)}
}

tempdata = lapply(hondadealerlink, getDealer.honda)
hondatemp = Reduce(function(x, y) rbind(x, y), tempdata)
hondazip = hondatemp$Zipcode
save(hondazip, file = "hondazip.rdata")


##########grab data through zip offered by dealerrater though honda oem website to get 
#the list


load("hondazip.rdata")

url = "http://automobiles.honda.com/tools/dealer-locator/results.aspx?address=&city=&state=&zip=10036&dealername=&filters=&fromsection=NEW#~pmZngNzcmx84O0"
zipcodelist = unique(hondazip)


tt = sapply(1:length(zipcodelist), function(i)gsub("zip=[0-9]{5}", paste0("zip=", zipcodelist[i]), url))


getcontent= function(node, content = "href"){
  tt = xmlAttrs(node)[content]
  return(tt)
}


getziplinks = function(url){
  print(url)
  if(url.exists(url)){
  doc = htmlParse(url)
  nodes = getNodeSet(doc, "//a[@id='dealerinfolink']")
  if(length(nodes)==0){return(NULL)}
  else{
    templink = strsplit(unname(sapply(nodes,getcontent,content='href')), "\\'")
    links = paste0("http://automobiles.honda.com/tools/dealer-locator/", sapply(templink, "[", 2))
    nlinks = substr(links, 1, 81)
    return(nlinks)
  }
  }
  else{return(NULL)}
}

links = sapply(tt, getziplinks)
ll = unique(as.vector(unlist(unname(links))))

getdealer = function(url){
  print(url)
  if(url.exists(url)){
  doc = htmlParse(url)
  tempname = xpathSApply(doc,"//div[@id='detail_dealername']",xmlValue)[1]
  index = tail(gregexpr("-", tempname)[[1]], n = 1)
  DealerName = gsub("^\\s+|\\s+$", "", substr(tempname, 1, index-1))
  Address = xpathSApply(doc,"//div[@id='dealer_address']",xmlValue)[1]
  Zipcode = tail(strsplit(Address, " ")[[1]], n = 1)
  Link = xpathSApply(doc, "//div[@id='detail_website1']/a", xmlValue)
  if(length(Link)==0){
    Link = "NA"
  }
  df <- data.frame(DealerName, Address, Link, Zipcode, stringsAsFactors = F)
  return(df)
  }
  else{return(NULL)}
}

tempdata = lapply(ll, getdealer)
HondaDealer = Reduce(function(x, y) rbind(x, y), tempdata)
names(HondaDealer)[4]="zipcode"
HondaDealers = merge(HondaDealer, zipdata)
HondaDealers$Latitude = "NA"
HondaDealers$Longitude = "NA"
HondaDealers$IV_link = "NA"
save(HondaDealers, file = "hondaDealers.rdata")

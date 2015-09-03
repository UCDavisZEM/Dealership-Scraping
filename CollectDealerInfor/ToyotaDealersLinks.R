####TOYOTA
install.packages("XML", repos = "http://cran.cnr.Berkeley.edu/")
install.packages("RCurl", repos = "http://cran.cnr.Berkeley.edu/")
library(RCurl)
library(XML)
url = "http://www.toyota.com/dealers/directory/"
doc = htmlParse(url)


getcontent= function(node, content = "href"){
  tt = xmlAttrs(node)[content]
  return(tt)
}

#get statelink(all-city)
baselink = "http://www.toyota.com"
statenode = getNodeSet(doc, "//li/a[@class='btn btn-circle tiny']")
statelink = paste0(baselink, unname(sapply(statenode,getcontent,content='href')))
statelink = gsub("index", "all-city", statelink)

#get each state's link
getcitylink = function(stateurl){
  baselink = "http://www.toyota.com"
  doc = htmlParse(stateurl)
  citynode = getNodeSet(doc, "//li/a[@class='btn btn-circle tiny']")
  citylink = paste0(baselink, unname(sapply(citynode,getcontent,content='href')))
  return(citylink)
}

citylinks   = unlist(unname(sapply(statelink, getcitylink)))

#get data from each city
getdealer = function(cityurl){
  print(cityurl)
  if(url.exists(cityurl)){
    doc = htmlParse(cityurl)
    dealernode = getNodeSet(doc, "//a[@data-dealername]")
    DealerName = unname(sapply(dealernode,getcontent,content='data-dealername'))
    if(length(DealerName)==0){
      return(NULL)
    }
    else{
      Zipcode = unname(sapply(dealernode,getcontent,content='data-zipcode'))
      Address = gsub("^>\\s+|\\s+$", "", xpathSApply(doc,"//span[@class='dealer-address']",xmlValue))
      Link = unname(sapply(getNodeSet(doc, "//a[@class='secondary-cta ']"),getcontent,content='href'))
      
      df <- data.frame(DealerName, Address, Link, Zipcode, stringsAsFactors = F)
      return(df)
    }
  }
  else{return(NULL)}
  
}

tt = lapply(citylinks, getdealer)
temp = Reduce(function(x, y) rbind(x, y), tt)
ToyotaDealer = temp[!duplicated(temp), ]

load("zipdata.rdata")
names(ToyotaDealer)[4]="zipcode"
ToyotaDealers = merge(ToyotaDealer, zipdata)
ToyotaDealers$Latitude = "NA"
ToyotaDealers$Longitude = "NA"
ToyotaDealers$IV_link = "NA"
save(ToyotaDealers, file = "toyotaDealers.rdata")

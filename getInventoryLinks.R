library(zipcode)
library(state)
data(zipcode)
data(state)
allzipcode = subset(zipcode, state %in% c(state.abb, "DC"))

MAzipcode = subset(allzipcode, state == "MA")
write.csv(file="MAzipcode.csv",x=MAzipcode,row.names=FALSE)

#exploration
sort(table(MAzipcode$city),decreasing = TRUE)
MAzipcode$zip[MAzipcode$city=='Boston']
dim(MAzipcode)
MAzipcode[,1]
MAzipcode$city[MAzipcode$zip=='01743']
zipcode$city[zipcode$zip=='06790']

#---------------------------------------------
# After we collect all the homepage links of local dealerships(eg.ChevroletMAdealers.csv), we want to get their inventory links directly through 
# through scraping on the Google result page

# First, we need to do some data cleaning for the file (eg.ChevroletMAdealers.csv).
ma_df = read.csv("FordMAdealers.csv",header=TRUE,stringsAsFactors=F)
head(ma_df)
length(unique(ma_df$Name))
#There is totally unique 97 results from Google Places API.
#We just need to extract the unique and useful information we need.

#Based on the result from the Google Places API, identify how many dealerships don't have websites
unusedSet1 = unique(ma_df$Name[ma_df$Website==''|ma_df$Website=='Nissan'])
unusedSet2 = unique(ma_df$Name[grep('repair|used|service|sales|truck|center|owned|department|associates',ma_df$Name,ignore.case=TRUE )])
unusedSet = c(unusedSet1, unusedSet2)

#Note that after we did a little research, we found that some results are usefulless because they are used car dealer, repair car shop,
#or even permanently colsed. 

library(dplyr)
maDealers <- tbl_df(ma_df)
maDealers <- maDealers[!duplicated(maDealers$Name),]
maDealersLinks= maDealers %>%
  filter(!(Name %in% unusedSet)) %>%
  select(Name,Website) %>%
  unique()

length(maDealersLinks$Name)
#Around 90 Chevrolet local dealerships(not exactly correct )
mainDealers = maDealersLinks$Name[grep("ford",maDealersLinks$Name,ignore.case=TRUE )] #name with chevrolet 43
length(mainDealers) #49 Toyota dealerships #35 Honda dealerships #25 Nissan dealerships
otherDealers = maDealersLinks$Name[-grep('nissan',maDealersLinks$Name,ignore.case=TRUE )]#name without chevrolet 47
length(otherDealers)
#write.csv(file='MADealersLinks.csv',x=maDealersLinks,row.names=FALSE)

#-----------------------------------------------
# Scrape the inventory links from Google search result page
library(RHTMLForms)
google= "http://www.google.com"

google_forms = getHTMLFormDescription(google)
#google_forms
gform = google_forms$f
#attributes(gform)
#gform$formAttributes

#create submitting function for google search Form
google_submit = createFunction(gform)

library(RCurl)
#test: google_submit function
#dealerInventory = google_submit("centralchevyauto.com view new inventory")
#r_project = google_submit("python")
#test = google_submit("hanleesdavischevy.com new inventory")

#XPath://h3/a to lool for Google results
get_google_page_urls <- function(html) {
  doc <- htmlParse(html)
  # extract url nodes using XPath. 
  attrs <- xpathApply(doc, "//h3//a[@href]", xmlAttrs)  
  # extract urls
  links <- sapply(attrs, function(x) x[[1]])  
  # free doc from memory
  free(doc)  
  # ensure urls start with "http" to avoid google references to the search page
  links <- grep("http://", links, fixed = TRUE, value=TRUE)
  return(links)
}

#function to clean the links returned by the "get_google_page_urls" function
url_trim <- function(urls){  
  urls = gsub('/url\\?q\\=|\\&sa.*','',urls) 
  #need to convert the html character
  urls = as.vector(sapply(urls,URLdecode))
  return(urls)
}

#function to construct inqueries strings
conSearchString <- function(str)
{
  #choose those keywords appearing in the inventory links
  if(grepl('www',str))
    str = gsub('.*www\\.(.*)','\\1',str)
  else
    str = gsub('http://(.*)','\\1',str)
  str = gsub('(.*com/).*','\\1',str)
  return(paste0("site:",str," new inventory vehicles"))
}

#integrated function to scrape on the first google search page
scrapeInventoryLink <- function(request)
{
  doc = google_submit(request)
  #set the longer system sleeping time to avoid the risk of being blocked by Google
  Sys.sleep(15)
  urls = get_google_page_urls(doc)
  urls = url_trim(urls)
  Sys.sleep(15)
  print(urls[1]) #for testing and debugging
  return(urls)
}

#The process 
maDealersLinks$Website[maDealersLinks$Name%in%mainDealers][48] = "http://baystateford.com/"
requests = as.vector(sapply(maDealersLinks$Website[maDealersLinks$Name%in%mainDealers],conSearchString))
inventoryLinks = lapply(requests, scrapeInventoryLink) #there are some unrelevant links in it

#function to choose clean and exact inventory links from inventoryLinks
clean_links <- function(links_ls)
{
  #print(length(grep("(/new-inventory)|(/new-vehicles)|(/new_inventory)|(SearchResults\\?search=new$)|(search/new)|(/searchnew)",links_ls,ignore.case=TRUE)))
  if(length(grep("(^http.*/new-inventory)|(^http.*/new-vehicles?)|(^http.*/new_inventory)|(^http.*SearchResults\\?search=new$)|(^http.*search/new)|(^http.*/searchnew)|(^http.*/new/)|(^http.*Cars$)|(^http.*condition=new)|(^http.*/new-cars)|(^http.*/inventory/$)|(^http.*/inventory\\.aspx$)",links_ls,ignore.case=TRUE))>=1)
    { 
      link = links_ls[grep("(^http.*/new-inventory)|(^http.*/new-vehicles?)|(^http.*/new_inventory)|(^http.*SearchResults\\?search=new$)|(^http.*search/new)|(^http.*/searchnew)|(^http.*/new/?$)|(^http.*Cars$)|(^http.*condition=new)|(^http.*/new-cars)|(^http.*/inventory/$)|(^http.*/inventory\\.aspx$)",
                           links_ls,ignore.case=TRUE)][1]
      if(nchar(link)<100)
        #print(nchar(link))
        return(link)
      else
        return(NA)  #return NA if links are too wield
    }
  else
  {
    if(TRUE%in%grepl('search=',links_ls))
    {
      link = links_ls[grep('search=',links_ls)][1]
      link = gsub('(search=).*','\\1',link)
      if(nchar(link)<100)
        return(paste0(link,'new'))
      else
        return(NA)
    }
    else if(TRUE%in%grepl('^http.*/used-inventory',links_ls))
    {
      link = links_ls[grep('^http.*/used-inventory',links_ls)][1]
      link = gsub('(^http.*/)used.*','\\1',link)
      return(paste0(link,'new-inventory/'))
    }
    else
      return(NA)  #return NA if we can't find the links
  }
}

#no need
checkNA <- function(df)
{
  missingDLs = df$Name[is.na(df$Website)]
  missingILs = maDealersLinks$Website[maDealersLinks$Name %in% missingDLs]
  requests = as.vector(sapply(missingILs,conSearchString))
  update_ILs = sapply(requests,function(request) 
                  {
                    Sys.sleep(10)
                    doc = google_submit(request)
                    urls = get_google_page_urls(doc)
                    Sys.sleep(10)
                    urls = url_trim(urls)
                    print(urls[1])
                    clean_links(urls)
                    }
         )
  df$Website[is.na(df$Website)] = update_ILs
}
#no need
verifyURL <- function(df)
{
  sapply(1:dim(df)[1], function(i)
                      {
                       symbol = gsub('(.*) nissan.*','\\1',df$Name[i],ignore.case=TRUE)  
                       symbol = gsub(' ','',symbol)
                       grepl(symbol,df$Website[i])   
  })
}

#test  7(used jeep),10(no website),16(permanently closed), 22(can be found), 40(can be found)
test = inventoryLinks[[52]]
length(grep("(/new-inventory)|(/new-vehicles?)|(/new_inventory)|(SearchResults\\?search=new$)|(search/new)|(/searchnew)|(/new/?$)|(Cars$)|(condition=new)",test,ignore.case=TRUE))
test[grep("(/new-inventory)|(/new-vehicles?)|(/new_inventory)|(SearchResults\\?search=new$)|(search/new)|(/searchnew)|(/new/?$)|(Cars$)|(condition=new)",test,ignore.case=TRUE)]
requests[52]
#11,36 n
#The complet list of authorized local dealerships' inventory links
authorized_ls = as.vector(sapply(inventoryLinks,clean_links))
#manually check NA
authorized_ls[11]="http://www.wallsford.com/inventory/" 
chevIL =df  #no 
chevIL = data.frame(Name=mainDealers,Website=authorized_ls,stringsAsFactors=FALSE)
#May Need to check those NA values manually and get rid of those unusual
chevIL$Website[11] = "http://www.townsendford.net/new-inventory/"
chevIL$Website[36] = "http://jackmaddenfordsales.com/Boston/For-Sale/New/" #get rid of 14
#Need to double check whether there are duplicates, and update any unusual
chevIL[duplicated(chevIL$Website),]
chevIL$Website[c(4,37,43,45,52,48)] = NA
chevILinks = na.omit(chevIL) 
dim(chevILinks)
write.csv(file='MAFordInventoryLinks.csv',x=chevILinks,row.names=FALSE)
df = chevIL

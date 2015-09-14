
#-----------------------------------------------
# Scrape the inventory links from Google search result page
install.packages("XML", repos = "http://cran.cnr.Berkeley.edu/")
install.packages("RHTMLForms", repos = "http://cran.cnr.Berkeley.edu/")
install.packages("RCurl", repos = "http://cran.cnr.Berkeley.edu/")
library(RHTMLForms)
library(XML)
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

check_case <- function(link)
{
  if(grepl("SearchResults\\?search=new$",link,ignore.case=T))#1
    case = "case1"
  else if(grepl("search/new/tp",link,ignore.case = T)) #2
    case = "case2"
  else if(grepl("new-inventory(/?)(\\??)$|(new-inventory/index\\.html?)$|(NewInventory\\.htm)",link,ignore.case=T)) #3
    case = "case3"
  else if(grepl("new_inventory",link,ignore.case=T))  #4
    case = "case4"
  else if(grepl("new-vehicles",link,ignore.case=T)) #5
    case = "case5"
  else if(grepl("searchnew\\.aspx$",link,ignore.case=T))#6
    case = "case6"
  else if(grepl("newsearch/new",link,ignore.case=T)) #7
    case = "case7"
  else if(grepl("condition=new",link,ignore.case=T)) #8
    case = "case8"
  else if(grepl("smart.*/new",link,ignore.case=T)) #9
    case = "case9"
  else if(grepl("For-sale/New/?$",link,ignore.case=T))#10
    case = "case10"
  else if(grepl("/new-cars-",link,ignore.case=T))#11
    case = "case11"
  else if(grepl("(/view/New/)$|/view/Make",link,ignore.case=T))#12
    case = "case12"
  else if(grepl("/NewToyotaCars$",link,ignore.case=T))#13
    case = "case13"
  else if(grepl("/NewToyotaCars.aspx",link,fixed=T))#14
    case = "case14"
  else if(grepl("/inventory/new-vehicles",link,fixed=T))#15
    case = "case15"
  else if(grepl("/New-Inventory.aspx",link,fixed=T))#16
    case = "case16"
  else if(grepl("vehicle_type=All",link,fixed=T))#17
    case = "case17"
  else if(grepl("/new-.*inventory.*aspx$",link,ignore.case=T))#19
    case = "case19"
  else if(grepl("/new-chevrolets.aspx$",link,ignore.case = T)) #same code with #19
    case = "case19"
  else if(grepl("(/inventory.aspx$)|(/new-cars.aspx$)",link)) #same code with #19
    case = "case19"
  else if(grepl("/Inventory/",link, fixed = T)) #20
    case = "case20"
  else if(grepl("/web/inventory/new",link, fixed = T)) #21
    case = "case21"
  else if(grepl("/new-cars/for-sale",link, fixed = T)) #22
    case = "case22"
  else if(grepl("com/new-toyota",link, fixed = T)) #23
    case = "case23" 
  else if(grepl("vehicle/search/new/$",link)) #24
    case = "case24" 
  else if(grepl("com/search/New+",link, fixed = T)) #25 http://www.norwalktoyota.com/search/New+Toyota+tm
    case = "case25" 
  else if(grepl("com/new-cars/?$",link)) #25 http://www.norwalktoyota.com/search/New+Toyota+tm
    case = "case26"
  else if(grepl("/inventory(\\?type=new)?$",link)) #27 http://www.claremonttoyota.com/inventory?type=new
    case = "case27"
  else if(grepl("/inventory/new/?$",link)) #28 http://www.claremonttoyota.com/inventory?type=new
    case = "case28"
  else
    case = "unknown"
  return(case)
}
# case  inventory?type=new http://www.jimnortontoyota.com/inventory?type=new
# case  http://www.toyotaoffayetteville.com/toyota/
# case  http://totoyota.com/New-Inventory-2.aspx

#function to choose clean and exact inventory links from inventoryLinks
clean_links <- function(links_ls)
{
  case_ls = unname(sapply(links_ls,check_case))
  if(length(case_ls[case_ls!="unknown"])>0)
  { 
    index = match(case_ls[case_ls!="unknown"],case_ls)
    if(length(index)>1 && nchar(links_ls[index[1]])<70)
      index = index[1]
    else if(length(index)>1 && nchar(links_ls[index[1]])>=70)
      index = index[2]
    else if(length(index)>2 && nchar(links_ls[index[2]])>=70)
      index = index[3]
    else
      index = index[1]
    
    link = links_ls[index]
    if(nchar(link)<90)
      #print(nchar(link))
      return(link)
    else
    {
        return(NA)  #return NA if links are too wield
    }      
  }
  else
  {
    if(TRUE%in%grepl('^http.*/used-inventory',links_ls))
    {
      link = links_ls[grep('^http.*/used-inventory',links_ls)][1]
      link = gsub('(^http.*/)used.*','\\1',link)
      return(paste0(link,'new-inventory/'))
    }
    else if(TRUE%in%grepl("^http.*\\.com/new-inventory/.*aspx",links_ls))
    {
      link = links_ls[grep('\\.com/new-inventory/.*aspx',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"New-Inventory.aspx")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*com/new-inventory/details?",links_ls,fixed=T))
    {
      link = links_ls[grep('com/new-inventory/details',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"new-inventory?vehicle_type=All")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*/new-inventory/",links_ls))
    {
      link = links_ls[grep('/new-inventory/',links_ls)][1]
      IV_link = gsub("(.*new-inventory/).*",'\\1',link)
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*\\.com/searchused.aspx",links_ls))
    {
      link = links_ls[grep('\\.com/searchused.aspx',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"searchnew.aspx")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*\\.com/auto/new-",links_ls))
    {
      link = links_ls[grep('\\.com/auto/new-',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"search/new/tp/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*VehicleSearchResults",links_ls))
    {
      link = links_ls[grep('VehicleSearchResults',links_ls)][1]
      IV_link = gsub("(.*VehicleSearchResults).*",'\\1',link)
      IV_link = paste0(IV_link,"?search=new")
      return(IV_link)
    }
    else if(TRUE%in%grepl("com/Vehicle_Details/desc/",links_ls,fixed=T))
    {
      link = links_ls[grep('\\.com/Vehicle_Details/desc/',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"New_Inventory/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*\\.com/inventory/",links_ls))
    {
      link = links_ls[grep('^http.*\\.com/inventory/',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"new-vehicles/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*/For-Sale/New/",links_ls))
    {
      link = links_ls[grep('/For-Sale/New/',links_ls)][1]
      IV_link = gsub("(.*/For-Sale/New/).*",'\\1',link)
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*com/New-.*/vd/",links_ls))
    {
      link = links_ls[grep('com/New-.*/vd/',links_ls,fixed=T)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"inventory/view/New/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*com/car/",links_ls))
    {
      link = links_ls[grep('.com/car/',links_ls,fixed=T)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"inventory/new-vehicles")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*com/New-",links_ls,ignore.case = F))
    {
      link = links_ls[grep('.com/New-',links_ls,fixed=T)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"inventory/newsearch/New/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*\\.com/new-New+",links_ls))
    {
      link = links_ls[grep('\\.com/new-New+',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"searchnew.aspx")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*/web/inventory/",links_ls))
    {
      link = links_ls[grep('/web/inventory/',links_ls)][1]
      IV_link = gsub("(.*/web/inventory/).*",'\\1',link)
      IV_link = paste0(IV_link,"New/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("^http.*\\.com/new/",links_ls))
    {
      link = links_ls[grep('\\.com/new/',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"new-inventory/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("com/NewToyotaCars/",links_ls,fixed=T))
    {
        link = links_ls[grep('com/NewToyotaCars/',links_ls)][1]
        IV_link = gsub("(.*\\.com/).*",'\\1',link)
        IV_link = paste0(IV_link,"NewToyotaCars")
        return(IV_link)
      }  
    else if(TRUE%in%grepl("com/newtoyotacars/",links_ls,fixed=T))
    {
      link = links_ls[grep('com/newtoyotacars/',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"NewToyotaCars.aspx")
      return(IV_link)
    }
    else if(TRUE%in%grepl("com/newtoyotacars/",links_ls,fixed=T))
    {
      link = links_ls[grep('com/newtoyotacars/',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"NewToyotaCars.aspx")
      return(IV_link)
    }
    else if(TRUE%in%grepl("com/vehicle-details/new-",links_ls,fixed=T))
    {
      link = links_ls[grep('com/vehicle-details/new-',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"inventory?type=new") #case 24
      return(IV_link)
    }
    else if(TRUE%in%grepl("\\.com/vehicle/",links_ls))
    {
      link = links_ls[grep('\\.com/vehicle/',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"inventory/condition=new/")
      return(IV_link)
    }
    else if(TRUE%in%grepl("toyota.*/new-cars/$",links_ls))
    {
      link = links_ls[grep('toyota.*/new-cars/$',links_ls)][1]
      IV_link = gsub("(.*\\.com/).*",'\\1',link)
      IV_link = paste0(IV_link,"toyota/") #case 25
      return(IV_link)
    }
    else
      {
        link = links_ls[grep('^http.*com/',links_ls)][1]
        link = gsub('(^http.*com/).*','\\1',link)
        trylink = paste0(link,'VehicleSearchResults?search=new')
        trylink2 = paste0(link,'new-inventory/index.htm')
        if(url.exists(trylink))
          return(trylink)
        else if(url.exists(trylink2))
          return(trylink2)
        else
          return(NA)  #return NA if we can't find the links
      }
  }
}

#integrated function to scrape on the first google search page
scrapeInventoryLink <- function(request)
{
  doc = google_submit(request)
  #set the longer system sleeping time to avoid the risk of being blocked by Google
  Sys.sleep(15)
  urls = get_google_page_urls(doc)
  urls = url_trim(urls)
  #IV_link = clean_links(urls)
  Sys.sleep(15)
  print(urls[1]) #for testing and debugging
  return(urls)
}

#The process
load("nissanDealers.rdata")
#maDealersLinks$Website[maDealersLinks$Name%in%mainDealers][48] = "http://baystateford.com/"
requests = as.vector(sapply(NissanDealers$Link,conSearchString))
inventoryLinks[351:400] = lapply(requests[351:400], scrapeInventoryLink) #there are some unrelevant links in it
save(inventoryLinks,file="possiblelinks.rdata")

#The complet list of authorized local dealerships' inventory links
authorized_ls = as.vector(sapply(inventoryLinks,clean_links))

ToyotaDealers$IV_link = authorized_ls
write.csv(ToyotaDealers, "ToyotaDealers.csv",row.names=F)
save(ToyotaDealers,file="toyotaDealers.rdata")

#test  7(used jeep),10(no website),16(permanently closed), 22(can be found), 40(can be found)
#links_ls = inventoryLinks[[]]
#length(grep("(/new-inventory)|(/new-vehicles?)|(/new_inventory)|(SearchResults\\?search=new$)|(search/new)|(/searchnew)|(/new/?$)|(Cars$)|(condition=new)",test,ignore.case=TRUE))
#test[grep("(/new-inventory)|(/new-vehicles?)|(/new_inventory)|(SearchResults\\?search=new$)|(search/new)|(/searchnew)|(/new/?$)|(Cars$)|(condition=new)",test,ignore.case=TRUE)]
#requests[19]
#11,36 n


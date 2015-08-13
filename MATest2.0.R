source("Case study-1.R")
source("Case study-2.R")
source("Case study-3.R")
source("Case study-4.R")
source("Case study-5.R")
source("Case study-6.R")
source("Case study-7.R")
source("Case study-8.R")
source("Case study-9(smart).R")
source("Case study-10.R")
source("Case study-11.R")
source("Case study-12.R")
source("Case study-13.R")
source("Case study-14.R")
source("Case study-15.R")
source("Case study-16.R")
source("Case study-17.R")
source("Case study-19.R")



link_file = read.csv("./DealerInventoryLinks//MANissanInventoryLinks.csv",header=TRUE,stringsAsFactors=FALSE)
links = link_file$Website


check_case <- function(link)
{
  if(grepl("SearchResults\\?search=new$",link,ignore.case=T))#1
      case = "case1"
  else if(grepl("search/new/tp",link,ignore.case = T)) #2
      case = "case2"
  else if(grepl("new-inventory(/?)$|(/index\\.html?)$",link,ignore.case=T)) #3
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
  else if(grepl("For-sale/new",link,ignore.case=T))#10
      case = "case10"
  else if(grepl("/new-cars-",link,ignore.case=T))#11
      case = "case11"
  else if(grepl("/view/New/",link,ignore.case=T))#12
      case = "case12"
  else if(grepl("/NewToyotaCars$",link,ignore.case=T))#13
      case = "case13"
  else if(grepl("/NewToyotaCars.aspx",link,fixed=T))#14
      case = "case14"
  else if(grepl("/inventory/new-vehicles",link,fixed=T))#15
      case = "case15"
  else if(grepl("/New-Inventory.aspx",link,ignore.case=T))#16
      case = "case16"
  else if(grepl("vehicle_type=All",link,fixed=T))#17
      case = "case17"
  else if(grepl("/new-.*-inventory-.*aspx",link,ignore.case=T))#19
      case = "case19"
  else if(grepl("/new-chevrolets.aspx$",link,ignore.case = T)) #same code with #19
      case = "case19"
  else
      case = "unknown"
  return(case)
}

getData <- function(link,case)
{  
  print(link)
  switch(case,
         case1 = alldata.1(link),
         case2 = alldata.2(link),
         case3 = alldata.3(link),
         case4 = alldata.4(link),
         case5 = alldata.5(link),
         case6 = alldata.6(link),
         case7 = alldata.7(link),
         case8 = alldata.8(link),
         case9 = alldata.9(link),
         case10 = alldata.10(link),
         case11 = alldata.11(link),
         case12 = alldata.12(link),
         case13 = alldata.13(link),
         case14 = alldata.14(link),
         case15 = alldata.15(link),
         case16 = alldata.16(link),
         case17 = alldata.17(link),
         case19 = alldata.19(link))
}
#  (^http.*Cars$)
  
#  (^http.*/new-cars)
  
case_ls = unname(sapply(links,check_case))
#to know which cases we should keep working on
link_file$Name[which(case_ls=="unknown")]
link_file$Website[which(case_ls=="unknown")]


alldata = mapply(getData,links[-which(case_ls=="unknown")],case_ls[-which(case_ls=="unknown")])
#class(alldata)
colnames(alldata) <- link_file$Name[-which(case_ls=="unknown")]

getDataframe <-function(alldata){
  lengths<-sapply(alldata[1,],length)
  data.frame(Dealership=rep(colnames(alldata),lengths),
             lapply(split(alldata,rownames(alldata)[row(alldata)]), unlist),
             row.names=NULL,stringsAsFactors = F)
}

#Dealership Dataframe
alldata_df = getDataframe(alldata)
nissan_df = alldata_df
head(nissan_df)
save(nissan_df,file = 'nissan.RData')


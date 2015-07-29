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

link_file = read.csv("./DealerInventoryLinks//MAChevInventoryLinks.csv",header=TRUE,stringsAsFactors=FALSE)
links = link_file$Website

check_case <- function(link)
{
  if(grepl("SearchResults\\?search=new$",link,ignore.case=T))#1
      case = "case1"
  else if(grepl("search/new/tp",link,ignore.case = T)) #2
      case = "case2"
  else if(grepl("new-inventory/",link,ignore.case=T)) #3
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
  else
      case = "unknow"
  return(case)
}

case_ls = unname(sapply(links,check_case))

#for test

getData <- function(link,case)
{ 
  if case =="unknown"
    return(NA)
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
         case12 = alldata.12(link))
}
#  (^http.*Cars$)
  
#  (^http.*/new-cars)
  
mapply(getData,links,case_ls)



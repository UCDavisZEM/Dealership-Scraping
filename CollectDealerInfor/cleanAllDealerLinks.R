ncheck_case <- function(link)
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
  else if(grepl("/inventory.aspx$",link)) #same code with #19
    case = "case19"
  else if(grepl("/Inventory/",link, fixed = T)) #20
    case = "case20"
  else if(grepl("/web/inventory/new",link, fixed = T)) #21
    case = "case21"
  else if(grepl("/new-cars/for-sale",link, fixed = T)) #22
    case = "case22"
  else if(grepl("/vehicle/search/new/\\?page=",link, fixed = T)) #23
    case = "case23" 
  else if(grepl("bmw-cars\\.",link)) #24
    case = "case24" 
  else if(grepl("/new-cars.aspx$",link, fixed = T)) #25 http://www.norwalktoyota.com/search/New+Toyota+tm
    case = "case25" 
  else
    case = "unknown"
  return(case)
}

BMW_case_list = unname(sapply(BMWDealers$IV_link,ncheck_case))
Chevrolet_case_list = unname(sapply(ChevroletDealers$IV_link,ncheck_case))
Ford_case_list = unname(sapply(FordDealers$IV_link,ncheck_case))
Honda_case_list = unname(sapply(HondaDealers$IV_link,ncheck_case))
Nissan_case_list = unname(sapply(NissanDealers$IV_link,ncheck_case))
Toyota_case_list = unname(sapply(ToyotaDealers$IV_link,ncheck_case))

#to know which cases we should keep working on
link_file$Name[which(case_ls=="unknown")]
link_file$Website[which(case_ls=="unknown")]
ncheck_case <- function(link)
{
  if(grepl("SearchResults\\?search=new$",link,ignore.case=T))#1
    case = "case1"
  else if(grepl("(search/new/tp)|(search/new-.*/tp)",link,ignore.case = T)) #2
    case = "case2"
  else if(grepl("new-inventory(/?)(\\??)|(new-inventory/index\\.html?)|(NewInventory\\.htm)|(all-inventory)",link,ignore.case=T)) #3
    case = "case3"
  else if(grepl("(new-.*-inventory.htm$)|(-bmw-inventory.htm$)",link,ignore.case = T)) #3
    case = "case3"
  else if(grepl("new_inventory",link,ignore.case=T))  #4
    case = "case4"
  else if(grepl("com/new-vehicles",link,ignore.case=T)) #5
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
  else if(grepl("(/new-cars-)|(com/new-bmw-)",link,ignore.case=T))#11
    case = "case11"
  else if(grepl("(/view/New/)$|(/view/Make)|(newsearch/Make)",link,ignore.case=T))#12
    case = "case12"
  else if(grepl("(/NewToyotaCars$)|(NewBMWCars$)",link,ignore.case=T))#13
    case = "case13"
  else if(grepl("(/NewToyotaCars\\.aspx)|(/NewBMWCars\\.aspx)",link))#14
    case = "case14"
  else if(grepl("/inventory/new-vehicles",link,fixed=T))#15
    case = "case15"
  else if(grepl("/New-Inventory.aspx",link,fixed=T))#16
    case = "case16"
  else if(grepl("vehicle_type=All",link,fixed=T))#17
    case = "case17"
  else if(grepl("/new-.*inventory.*aspx$",link,ignore.case=T))#19
    case = "case19"
  else if(grepl("(/new-chevrolets.aspx$)|(/bmw.aspx)|(/new-bmws.aspx)",link,ignore.case = T)) #same code with #19
    case = "case19"
  else if(grepl("(/inventory.aspx$)|(com/bmw/$)",link)) #same code with #19
    case = "case19"
  else if(grepl("/Inventory/",link, fixed = T)) #20
    case = "case20"
  else if(grepl("/web/inventory/new",link, fixed = T)) #21
    case = "case21"
  else if(grepl("/new-cars/for-sale",link, fixed = T)) #22
    case = "case22"
  else if(grepl("/vehicle/search/new/(\\?page=)?",link)) #23
    case = "case23" 
  else if(grepl("(bmw-cars\\.asp$)|(bmw\\.asp$)",link)) #24
    case = "case24" 
  else if(grepl("(/new-cars\\.aspx$)|(bmw-cars\\.aspx$)",link)) #25 
    case = "case25" 
  else if(grepl("new\\.php",link))  #26
    case = "case26"
  else if(grepl("plugin-inventory",link)) #27
    case = "case27"
  else if(grepl("/all/all",link)) #28
    case = "case28"
  else if(grepl("newinventory\\.aspx",link)) #29
    case = "case29"
  else if(grepl("/new\\.cfm",link))  #30
    case = "case30"
  else if(grepl("/inventory?type=new",link,fixed=T))  #31
    case = "case31" 
  else if(grepl("com/vehicle/inventory/new$",link))  #32
    case = "case32"
  else if(grepl("com/new/inventory$",link))  #33
    case = "case33"
  #else if(grepl("/new-cars$",link))  
  #  case = "case34"
  else if(grepl("jcbmwnew.htm",link,fixed=T))  #35
    case = "case35"
  else if(grepl("/search/s:yr1/?q=&tp=new&mk=8-bmw/",link,fixed=T))  #36 special
    case = "case36"
  else if(grepl("new_bmw.cfm",link,fixed=T))  #37
    case = "case37"
  else if(grepl("/brands/bmw/new/inventory",link,fixed=T))  #38 special
    case = "case38"
  else if(grepl("browse-new-bmw-inventory",link,fixed=T))  #40
    case = "case40"
  else if(grepl("selectedcat=new$",link))  #42
    case = "case42"
  else
    case = "unknown"
  return(case)
}

BMWDealers[] <- lapply(BMWDealers, as.character)
save(BMWDealers,file="nBMWDealers.rdata")

BMW_case_list = unname(sapply(BMWDealers$IV_link,ncheck_case))
Chevrolet_case_list = unname(sapply(ChevroletDealers$IV_link,ncheck_case))
Ford_case_list = unname(sapply(FordDealers$IV_link,ncheck_case))
Honda_case_list = unname(sapply(HondaDealers$IV_link,ncheck_case))
Nissan_case_list = unname(sapply(NissanDealers$IV_link,ncheck_case))
Toyota_case_list = unname(sapply(ToyotaDealers$IV_link,ncheck_case))

#to know which cases we should keep working on
BMWDealers$DealerName[which(BMW_case_list=="unknown")] 
BMWDealers$IV_link[which(BMW_case_list=="unknown")] 

ToyotaDealers$IV_link[which(Toyota_case_list=="unknown")] 
BMWDealers$DealerName[which(BMWDealers$IV_link=='http://www.erhardbmwoffarmingtonhills.com/Specials/new')] = "http://www.erhardbmwoffarmingtonhills.com/VehicleSearchResults?search=new"

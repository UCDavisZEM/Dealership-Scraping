#integrte the location information for each dealership
load("new-madata.rdata") #example data 
dealer_ls = unique(madata$Dealership)
length(dealer_ls)
require(dplyr)

all_csvfiles = list.files(path = './Dealers information/',pattern = '.csv')
allfiles_str = unname(sapply(all_csvfiles,function(dir_str) paste0('./Dealers information/',dir_str)))
dealerinfo = ldply(allfiles_str, function(file_name) read.csv(file = file_name,header = T,stringsAsFactors=F)[,c('Name','Adress','GeoLatitude','GeoLongtitude')])
names(dealerinfo) =c('Name','Address','GeoLatitude','GeoLongitude') 

dim(dealerinfo) 
#filter applicable and unduplicated rows
dealerinfo <- dealerinfo  %>%
  filter(Name %in% dealer_ls) %>%
  filter(!duplicated(Name))
#extract the zipcode for each dealerships
dealerinfo$zipcode = gsub('.*([0-9]{5}).*','\\1',dealerinfo$Address)
dealerinfo$state = gsub('.* (.*) [0-9]{5}.*','\\1',dealerinfo$Address)
head(dealerinfo)

#check whether there is any exceptions
as.numeric(dealerinfo$zipcode)
#index with 132 has a warning about NA
dealerinfo[132,]$Address = "765 Memorial Dr, Chicopee, MA 01020"
dealerinfo[132,]$zipcode = gsub('.*([0-9]{5}).*','\\1',dealerinfo[132,]$Address)
dealerinfo[132,]$state = gsub('.* (.*) [0-9]{5}.*','\\1',dealerinfo[132,]$Address)

save(dealerinfo,file='dealerinfo.rdata')

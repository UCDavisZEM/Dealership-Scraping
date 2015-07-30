###SOLVED
#Special case--
#url = "http://www.quirkchevy.com/new-vehicles/"
#url ="http://www.hondanorth.com/new-vehicles/"
#Not solved
#url = "http://www.silkohonda.com/new-vehicles/" 
#url = "http://www.primetoyotacars.com/inventory/new-vehicles"
#

alldata.5 = function(url){
  require(XML)
  doc = htmlParse(url)
  txt = xpathSApply(doc, "//script[@type='text/javascript']",xmlValue)  
  txt = txt[grep("inventory_localization",txt)]
  vin_str = gsub('.*vin_numbers.*\\[(.*?)\\].*','\\1',txt)
  vin_str = gsub('\"','', unlist(strsplit(vin_str,',')))
  
  make_str = gsub('.*vehicle_makes.*\\[(.*?)\\].*','\\1',txt)
  make_str = gsub('\"','', unlist(strsplit(make_str,',')))
  
  models_str = gsub('.*models.*\\[(.*?)\\].*','\\1',txt)
  models_str = gsub('\"','', unlist(strsplit(models_str,',')))
  
  year = rep(NA, length(vin_str))
  trim = rep(NA, length(vin_str))
  df <- data.frame(vin_str,make_str,models_str,trim, year,stringsAsFactors = F)
  colnames(df) <- c("VIN", "Make", "Model","Trim", "Year")
  return(df)
}


#test case smart  
url = "http://www.smartcenterboston.com/new"

scrapeInfo.9 <- function(links)
{
  vins = unique(gsub(".*-([0-9A-Z]{17})$",'\\1',links))
  make = gsub('-',' ',gsub(".*9999/(.*?)/.*",'\\1',links))
  model = gsub('-|(%20)', ' ',gsub(".*9999/(.*?)/(.*)/.*",'\\2',links))
  trim = rep(NA,length(links))
  year = rep(NA,length(links))
  df <- data.frame(vins,make,model,trim, year)
  colnames(df) <- c("VIN", "Make", "Model","Trim", "Year")
  return(df)
} 

alldata.9 = function(url){
  doc = htmlParse(url)
  links = getHTMLLinks(doc)
  vlinks = links[grep('/new/9999/',links)]
  cardata = scrapeInfo.9(vlinks)
  return(cardata)
}

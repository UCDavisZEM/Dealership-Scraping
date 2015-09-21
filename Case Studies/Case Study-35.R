#special
#Case Study 35

#url = "http://www.joelconferbmw.com/jcbmwnew.htm"

alldata.35 = function(url){
  
    doc = htmlParse(url)
    node = getNodeSet(doc, "//p")
    
    vins = unique(gsub(".*\\(([0-9A-z]{17})\\/.*","\\1",xmlApply(node, xmlValue)))
    vins =  vins[nchar(vins)==17]
    make = "BMW"
    model = "NA"
    trim = "NA"
    year = NA
    df <- data.frame(vins,make,model,trim,as.numeric(year), stringsAsFactors = F)
    colnames(df) <- c("VIN", "Make", "Model", "Trim", "Year")
    return(df)
  
}


#cc = alldata.35(url)
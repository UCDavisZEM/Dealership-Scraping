#VIN Decoder web scraping from  http://www.vindecipher.com/veyance/index.cfm?action=decode

load('df.RData')

library(RHTMLForms)
library(RCurl)

#url = "http://www.decodethis.com/VIN-Decoded/vin/"
url = "http://www.vindecipher.com/veyance/index.cfm?action=decode"

decoder_forms = getHTMLFormDescription(url)
dform = decoder_forms$valvinForm
#attributes(dform)
decoder_submit = createFunction(dform) 

#h = getCurlHandle(cookiejar = "")
#txt = postForm(url,vinNum='1G1YB2D73F5109332',submitBtn='Get VIN Data',curl=h,style="post",
#               .opts = curlOptions(
#                 referer="http://www.vindecipher.com/veyance/",
#                 useragent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.134 Safari/537.36"
#                 ))


#filter the VIN list for which we need to scrape for missing information,i.e, model, make, year
#may not necessary


#Web scraping function from the decoder
vin_decoder<- function(vin_str)
{
    #url = "http://www.decodethis.com/VIN-Decoded/vin/1FADP5CU0FL116000"
    vin_result = decoder_submit(vin_str)
    doc = htmlParse(vin_result)
    Sys.sleep(2)
    #/html/body/table[2]/tbody/tr[1]/td/table/tbody/tr[3]/td/table/tbody/tr[1]/td[2]
    infoNode = getNodeSet(doc,"//table/tr[@bgcolor]/td[2]/text()")
    Manufacturer = xmlValue(infoNode[[1]])
    Make = xmlValue(infoNode[[2]])
    Year = xmlValue(infoNode[[3]])
    Model = xmlValue(infoNode[[4]])
    
    print(paste(Manufacturer,Make,Year,Model,vin_str))
    return(c(Manufacturer,Make,Year,Model,vin_str))
}

vin_requests = unname(sapply(df$VIN,toString))
allcar_ls = lapply(vin_requests[1:20],vin_decoder) 
allcars_df = do.call('rbind',allcar_ls)

save(allcars_df,file='allcars_df.RData')

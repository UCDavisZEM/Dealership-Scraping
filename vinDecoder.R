#VIN Decoder web scraping from  http://www.vindecipher.com/veyance/index.cfm?action=decode

load('df.RData')
install.packages("devtools")
install.packages("Rcurl")
install.packages("XML")
install.packages("plyr")
library(devtools)
devtools::install_github("omegahat/RHTMLForms")
library(RHTMLForms)
library(XML)
library(RCurl)
library(plyr)

#url = "http://www.decodethis.com/VIN-Decoded/vin/"
url = "http://www.vindecipher.com/veyance/index.cfm?action=decode"

decoder_forms = getHTMLFormDescription(url)
dform = decoder_forms$valvinForm
#attributes(dform)
decoder_submit = createFunction(dform) 

#Approach2
#h = getCurlHandle(cookiejar = "")
#txt = postForm(url,vinNum='1G1YB2D73F5109332',submitBtn='Get VIN Data',curl=h,style="post",
#               .opts = curlOptions(
#                 referer="http://www.vindecipher.com/veyance/",
#                 useragent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.134 Safari/537.36"
#                 ))


#filter the VIN list for which we need to scrape for missing information,i.e, model, make, year
#actually not necessary


#Web scraping function from the decoder
vin_decoder<- function(vin_str)
{
    vin_result = decoder_submit(vin_str)
    doc = htmlParse(vin_result)
    #/html/body/table[2]/tbody/tr[1]/td/table/tbody/tr[3]/td/table/tbody/tr[1]/td[2]
    infoNode = getNodeSet(doc,"//table/tr[@bgcolor]/td[2]/text()")
    Manufacturer = xmlValue(infoNode[[1]])
    Make = xmlValue(infoNode[[2]])
    Year = xmlValue(infoNode[[3]])
    Model = xmlValue(infoNode[[4]])
    #print(paste(Manufacturer,Make,Year,Model,vin_str))
    rm(doc)
    return(data.frame(Manufacturer,Make,Year,Model,vin_str,stringsAsFactors = F))
}

vin_requests = unname(sapply(df$VIN,toString))
#allcar_ls = lapply(vin_requests,vin_decoder) 
#allcars_df = do.call('rbind',allcar_ls)
allcars_df = ldply(vin_requests, function(vin_str){
                                      #print(vin_str)
                                      out = try(vin_decoder(vin_str))
                                        if(class(out)=='try-error')
                                        {
                                          print(paste('Error VIN:',vin_str))
                                          next
                                        }
                                      #random sleep interval
                                      Sys.sleep(sample(seq(1, 4, by=0.001), 1))
                                      return(out)
                                    })
colnames(allcars_df) <- c("Manufacturer","Make","Year","Model","VIN")
save(allcars_df,file='allcars_df.RData')

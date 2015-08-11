#Load all the data collected and Deployment in a RShiny App
load('toyota.RData')
load('smart.RData')
load('chevrolet.RData')
load('honda.RData')
load('ford.RData')
load('nissan.RData')

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

OEM_list = list(nullToNA(chev_df),nullToNA(toyota_df),nullToNA(smart_df),nullToNA(honda_df),nullToNA(ford_df),nullToNA(nissan_df))
df <- Reduce(function(x, y) rbind(x, y), OEM_list)
df <- df[!duplicated(df$VIN),]

dim(df)
save(df,file='df.RData')
library(shiny)
runApp()

library(shinyapps)
deployApp()

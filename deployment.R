#Load all the data collected and Deployment in a RShiny App
load('toyota.RData')
load('smart.RData')
load('chevrolet.RData')

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

OEM_list = list(nullToNA(chev_df),nullToNA(toyota_df),nullToNA(smart_df))
df <- Reduce(function(x, y) rbind(x, y), OEM_list)
dim(df)
save(df,file='df.RData')
library(shiny)
runApp()

library(shinyapps)
deployApp()

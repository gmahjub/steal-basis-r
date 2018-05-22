dependencies <- c(
  "htmltools",
  "colorspace",
  "tidyverse",
  "tidyquant",
  "tidyr",
  "mnormt",
  "plyr",
  "lubridate",
  "yaml",
  "quantmod",
  "xts",
  "zoo",
  "PerformanceAnalytics",
  "alphavantager",
  "TTR",
  "dygraphs",
  "purrr",
  "purrrlyr",
  "dplyr",
  "ggplot2",
  "rlang",
  "pracma",
  "IBrokers",
  "timetk",
  "rpart",
  "rpart.plot",
  "mosaic",
  "corrr",
  "XLConnect",
  "plotly"
  )

## install packages that are not currently installed.

new.packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])] # get list of installed packages
if(length(new.packages)>0) {
  install.packages(new.packages)
}
lapply(dependencies, library, character.only = TRUE)

sourceDir <- function(path, trace = TRUE, ...){
  for (fl in list.files(path, pattern = "[.][RrSsQq]$")) {
    if (trace)
      cat(fl,":")
    source(file.path(path, fl), ...)
    if (trace) cat("\n")
  }
}

rm(dependencies, new.packages)

## set some variables that are commonly used, based on system we are running on (MAC or windows)

if (Sys.info()['sysname'] == "Darwin"){
  source(paste(getwd(), 'R/write_ticker_csv.R', sep = "/"))
  source(paste(getwd(), 'R/extend_yahoo_quantmod.R', sep = "/"))
  source(paste(getwd(), 'R/extend_av_quantmod.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  api_key_file<-paste(getwd(), "../../../data/ghazy_mahjub_api_keys.csv", sep = "/")
  yahoo_stock_prices_dir<-paste(getwd(), "../../../data/yahoo/", sep = "/")
  av_stock_prices_dir<-paste(getwd(), "../../../data/alphavantage/", sep = "/")
} else {
  source(paste(getwd(), 'R/write_ticker_csv.R', sep = "/"))
  source(paste(getwd(), 'R/extend_yahoo_quantmod.R', sep = "/"))
  source(paste(getwd(), 'R/extend_av_quantmod.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  api_key_file<-paste(getwd(), "..\\..\\..\\..\\data\\ghazy_mahjub_api_keys.csv", sep = "\\")
  yahoo_stock_prices_dir<-paste(getwd(), "..\\..\\..\\..\\data\\yahoo\\", sep = "\\")
  av_stock_prices_dir<-paste(getwd(), "..\\..\\..\\..\\data\\alphavantage\\daily\\", sep ="\\")
}
start_date<-"2017-01-01"
reserved_R_strings <-c("T", "F", "NA", "NULL", "C")

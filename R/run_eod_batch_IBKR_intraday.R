library(IBrokers)
library(xts)
library(zoo)
library(tidyquant)
library(tidyverse)
library(timetk)

if (Sys.info()['sysname'] == "Darwin"){
  source(paste(getwd(), 'R/write_ticker_csv.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  source(paste(getwd(), 'R/extend_IBrokers.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  eod_batch_IBKR_intraday(paste(getwd(), '../../../data/IBKR/intraday/stock.prices/', sep = "/"), port_number = 4001)
  
} else {
  source(paste(getwd(), 'R/write_ticker_csv.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  source(paste(getwd(), 'R/extend_IBrokers.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  eod_batch_IBKR_intraday(paste(getwd(), '..\\..\\..\\..\\data\\IBKR\\intraday\\stock.prices\\', sep = "\\"), port_number = 4001)
}

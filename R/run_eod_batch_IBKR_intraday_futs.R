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
  fut_prices_dir <- paste(getwd(), '../../../data/IBKR/intraday/fut.prices/', sep = "/")
  eod_batch_IBKR_intraday(fut_prices_dir, asset_class = "Futures", port_number = 7496, error_log_file_name = "IBKRgetHist_Futs_FailStatus.csv")
  
} else {
  source(paste(getwd(), 'R/write_ticker_csv.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  source(paste(getwd(), 'R/extend_IBrokers.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  fut_prices_dir <- paste(getwd(), '..\\..\\..\\..\\data\\IBKR\\intraday\\fut.prices\\', sep = "\\")
  eod_batch_IBKR_intraday(fut_prices_dir, asset_class = "Futures", port_number = 7496, error_log_file_name = "IBKRgetHist_Futs_FailStatus.csv")
}

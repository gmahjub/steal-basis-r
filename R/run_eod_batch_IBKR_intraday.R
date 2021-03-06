library(IBrokers)
library(xts)
library(zoo)
library(tidyquant)
library(tidyverse)
library(timetk)

FUTURE_ASSET_CLASS<-"FUTURES"
EQUITY_ASSET_CLASS<-"EQUITIES"
FOREX_ASSET_CLASS<-"FX"

if (Sys.info()['sysname'] == "Darwin"){
  source(paste(getwd(), 'R/write_ticker_csv.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  source(paste(getwd(), 'R/extend_IBrokers.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  stock_prices_dir<-paste(getwd(), '../../../data/IBKR/intraday/stock.prices/', sep = "/")
  eod_batch_IBKR_intraday(stock_prices_dir, asset_class = EQUITY_ASSET_CLASS, port_number = 4001)
} else {
  source(paste(getwd(), 'R/write_ticker_csv.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  source(paste(getwd(), 'R/extend_IBrokers.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  stock_prices_dir<-paste(getwd(), '..\\..\\..\\..\\data\\IBKR\\intraday\\stock.prices\\', sep = "\\")
  eod_batch_IBKR_intraday(stock_prices_dir, asset_class = EQUITY_ASSET_CLASS, port_number = 4001)
}

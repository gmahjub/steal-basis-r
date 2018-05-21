## call this script with Rscript at command line to initiate Alphavantage daily intraday tick pull.
## also, we can call this script via a batch process, or in a shell script to be automated on a daily basis.
library(xts)
library(zoo)
library(tidyquant)
library(alphavantager)
library(IBrokers)
library(timetk)

FUTURE_ASSET_CLASS<-"FUTURES"
EQUITY_ASSET_CLASS<-"EQUITIES"
FOREX_ASSET_CLASS<-"FX"

if (Sys.info()['sysname'] == "Darwin") {
  source(paste(getwd(), 'R/write_intraday_av.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  source(paste(getwd(), 'R/extend_IBrokers.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  path_to_api_key_file<-paste(getwd(), '../../../data/ghazy_mahjub_api_keys.csv', sep = "/")
  stock_prices_dir<-paste(getwd(), '../../../data/alphavantage/intraday/stock.prices/', sep = "/")
} else {
  source(paste(getwd(), 'R/write_intraday_av.R', sep = "/"))
  source(paste(getwd(), 'R/secure_api_dataAccess.R', sep = "/"))
  source(paste(getwd(), 'R/extend_IBrokers.R', sep = "/"))
  source(paste(getwd(), 'R/local_data_access.R', sep = "/"))
  path_to_api_key_file<-paste(getwd(), '..\\..\\..\\..\\data\\ghazy_mahjub_api_keys.csv', sep = "\\")
  stock_prices_dir<-paste(getwd(), '..\\..\\..\\..\\data\\alphavantage\\intraday\\stock.prices\\', sep = "\\")
}
eod_batch_av_intraday(path_to_ticker_dir = stock_prices_dir, path_to_api_key_file = path_to_api_key_file)



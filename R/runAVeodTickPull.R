## call this script with Rscript at command line to initiate Alphavantage daily intraday tick pull.
## also, we can call this script via a batch process, or in a shell script to be automated on a daily basis.
library(xts)
library(zoo)
library(tidyquant)
library(alphavantager)
library(IBrokers)
library(timetk)
source('~/workspace/stealthebasis/Rsrc/RstealBasis/R/secure_api_dataAccess.R')
source('~/workspace/stealthebasis/Rsrc/RstealBasis/R/extend_IBrokers.R')
source('~/workspace/stealthebasis/Rsrc/RstealBasis/R/write_intraday_av.R')
source('~/workspace/stealthebasis/Rsrc/RstealBasis/R/local_data_access.R')
eod_batch_av_intraday("/Users/traderghazy/workspace/data/alphavantage/intraday/stock.prices/", 
                      "/Users/traderghazy/workspace/data/ghazy_mahjub_api_keys.csv")



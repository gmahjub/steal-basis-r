install.packages("tidyquant")
install.packages("tidyverse")
install.packages("quantmod")
install.packages("xts")
install.packages("zoo")
install.packages("PerformanceAnalytics")
install.packages("alphavantager")
install.packages("TTR")
install.packages("dygraphs")
install.packages("purrr")
install.packages("purrrlyr")
install.packages("rlang")
install.packages("pracma")
install.packages("IBrokers")

library(tidyquant)
library(tidyverse)
library(quantmod)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(alphavantager)
library(TTR)
library(dygraphs)
library(purrr)
library(purrrlyr)
library(rlang)
library(pracma)
library(IBrokers)

if (Sys.info()['sysname'] == "Darwin"){
  source('/Users/ghazymahjub/workspace/stealthebasis/Rsrc/RstealBasis/R/write_ticker_csv.R')
  source('/Users/ghazymahjub/workspace/stealthebasis/Rsrc/RstealBasis/R/extend_yahoo_quantmod.R')
  yahoo_stock_prices_dir<-'/Users/ghazymahjub/workspace/data/yahoo'
} else {
  source('C:/Users/ghazy/workspace/stealthebasis/Rsrc/RstealBasis/RstealBasis/R/write_ticker_csv.R')
  source('C:/Users/ghazy/workspace/stealthebasis/Rsrc/RstealBasis/RstealBasis/R/extend_yahoo_quantmod.R')
  yahoo_stock_prices_dir<-"C:\\Users\\ghazy\\workspace\\data\\yahoo\\"
}
start_date<-"2017-01-01"

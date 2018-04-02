#' get_benchmark_period_returns
#'
#' Returns the period returns for the benchmark tickers listed in the
#' etf_to_sector_mapping_tibble. We are using quantmod functionality here, not
#' tidyquant. Returned from this function is an xts object, not a tibble object.
#' Ideally, we would want to be able to specify source and not hardcode yahoo as
#' source. Stock price time series is downloaded using quantmod functionality
#' (getSymbols) where the wrapper we are using here is
#' "yahoo_Main_retrieve_and_write()" function.
#'
#' @param etf_to_sector_mapping_tibble etf name to sector name mapping data object, tibble format.
#' @param periodicity options are: daily, weekly, monthly, quarterly, yearly
#' @param start_date in string format of: YYYY-MM-DD
#'
#' @return an xts object with the benchmark etf returns.
#' @export
#'
#' @examples
get_benchmark_period_returns<-function(etf_to_sector_mapping_tibble, yahoo_stock_prices_dir, start_date="2017-11-01", periodicity="daily"){
  tickers<-etf_to_sector_mapping_tibble$ticker
  # download prices using getSymbol, this is getting the data from yahoo
  # we should create another version to bet the data from alphavantage
  symbols<-yahoo_Main_retrieve_and_write(tickers, yahoo_stock_prices_dir, start_date = start_date)
  #symbols<- getSymbols(tickers, auto.assign = TRUE, warnings = FALSE, from=start_date)
  etf_prices<- do.call(merge, lapply(symbols, function(x) Ad(get(x))))
  etf_returns<- do.call(merge, lapply(etf_prices, function(x) periodReturn(x, period=periodicity, type='log')))
  colnames(etf_returns)<-paste(etf_to_sector_mapping_tibble$sector, etf_to_sector_mapping_tibble$ticker,sep=".")
  return(etf_returns)
}

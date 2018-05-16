#' check_Rcompat_ticker_names
#'
#' Takes a ticker string and removes any R incompatible text (reserved keywords
#' & symbols). Renames the ticker, pulls data on the ticker using getSymbols()
#' from quantmod. Calls adjustYahooOHLC() method to adjust the OHL prices in the
#' same manner the close price. Renames the columns prior to write out to csv so
#' that column names don't have any R incompatible characters. Calls the
#' write_ticker_csv function to write the ticker timeseries to file.
#'
#' @param ticker ticker as a string
#' @param path_to_ticker_dir path to directory where to write csv file out.
#' @param start_date what date to start the tick pull from when the getSymbols()
#'   quantmod func is called.
#'
#' @return the ticker as a string if it was modified, NA if it was not modified.
#' @export
#'
#' @examples
check_Rcompat_ticker_names <- function(ticker, path_to_ticker_dir, start_date="2007-01-01", end_date = Sys.Date(), write_file = FALSE) {
  # example path below
  changed_ticker<-NA
  require(stringr)
  tf<-str_detect(string=ticker, pattern=fixed("-"))
  if (tf) {
    m <- str_replace(string=ticker, pattern=fixed("-"), replacement=".")
    do.call("<<-", list(m, get(getSymbols(ticker, from=start_date, to=end_date))))
    col_names<-colnames(get(m))
    vec_new_col_nm<-c()
    for(name in col_names){
      new_col_name <-str_replace(string=name, pattern=fixed("-"), replacement=".")
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    changed_ticker<-ticker
    adjustOHLC_wrapper(m)
    if (write_file){
      write_ticker_csv(m, path_to_ticker_dir, column_names=vec_new_col_nm)
    }
  }
  ticker_confused_with_bool <- c('C', 'T', 'F', 'NA')
  if(ticker %in% ticker_confused_with_bool){
    m <- paste(ticker, ".", ticker, sep='')
    do.call("<<-", list(m, get(getSymbols(ticker, from=start_date, to=end_date))))
    col_names<-colnames(get(m))
    vec_new_col_nm<-c()
    for (name in col_names){
      new_col_name <- paste(ticker, ".", name, sep='')
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    changed_ticker<-ticker
    adjustOHLC_wrapper(m)
    if (write_file){
      write_ticker_csv(m, path_to_ticker_dir, column_names = vec_new_col_nm)
    }
  }
  return(changed_ticker)
}

#' adjustOHLC_wrapper
#'
#' AdjustOHLC in quantmod is NOT reliable. Yahoo data is currently all split
#' adjusted, but only the adjusted close is split AND dividend adjusted. Here we
#' are downloading the dividend data and doing the ratio calc ourselves. We are
#' downloading the dividend data from Yahoo! as source.
#'
#' @param tickers list of tickers, strings, for whose prices will be adjusted
#'   based on the close-adjusted adjustment.
#'
#' @return nothing is returned, the existing xts objects are updated to reflect
#'   the adjusted prices, OHLC, plus a sanity check, ticker.AdjDiff to show us
#'   the difference between Yahoo's adjusted close and our calculated close with
#'   divident adjustment,.
#' @export
#'
#' @examples
adjustOHLC_wrapper<-function(tickers){
  # yahoo data is entirely split adjusted, the only thing we need to factor in is the dividend adjustment
  for (ticker in tickers){
    dividends<-getDividends(ticker)
    ratios<-adjRatios(dividends = dividends, close = Cl(get(ticker)))
    cl_price<-Cl(get(ticker))
    op_price<-Op(get(ticker))
    hi_price<-Hi(get(ticker))
    lo_price<-Lo(get(ticker))
    adjusted_cl<-cl_price*ratios[,"Div"]
    adjusted_op<-op_price*ratios[,"Div"]
    adjusted_hi<-hi_price*ratios[,"Div"]
    adjusted_lo<-lo_price*ratios[,"Div"]
    sanity_check<-(adjusted_cl/Ad(get(ticker)) - 1)*100.0
    adjusted_data<-cbind(adjusted_op, adjusted_hi, adjusted_lo, adjusted_cl, Vo(get(ticker)), Ad(get(ticker)), sanity_check)
    adjusted_data<-na.omit(adjusted_data)
    col_names<-c(colnames(get(ticker)), paste(ticker, "AdjDiff", sep="."))
    names(adjusted_data) <- col_names
    do.call("<<-", list(ticker, adjusted_data))
  }
}

#' yahoo_Main_retrieve_and_write
#'
#' Main function for retrieving time series data from yahoo finance.
#'
#' @param tickers list of tickers to retrieve data objects (xts/zoo) for.
#' @param start_date start_date of the time series data to retrieve.
#' @param yahoo_stock_prices_dir yahoo stock price data directory.
#'
#' @return a list of ticker
#' @export
#'
#' @examples
yahoo_Main_retrieve_and_write<-function(tickers, yahoo_stock_prices_dir, start_date="2007-01-01", end_date = Sys.Date(), write_file = TRUE){
  path_to_ticker_dir <- yahoo_stock_prices_dir
  list_of_changed_tickers<-sapply(tickers, FUN=check_Rcompat_ticker_names, path_to_ticker_dir, start_date=start_date, end_date = end_date, write_file=write_file)
  list_of_changed_tickers<-list_of_changed_tickers[!is.na(list_of_changed_tickers)]
  tickers<-setdiff(tickers, list_of_changed_tickers)
  for (ticker in tickers){do.call("<<-", list(ticker, get(getSymbols(ticker, from=start_date))))}
  adjustOHLC_wrapper(tickers)
  lapply(ticker, subset_yahoo_xts, from = start_date, to = end_date)
  if (write_file) {
    lapply(tickers, FUN=write_ticker_csv, path_to_ticker_dir = path_to_ticker_dir)
  }
  ## there is a known bug in here, we do not include in this list the tickers that had to be changed due to R incompaitibility
  ## GM - 2/6/2018
  return(tickers)
}

#' getYahooDaily_xts
#'
#' Check to see if we have the ticker on local disk. Then, check to see if it is
#' up to date. How to check for up to date? Assume 6:00 pm is when Yahoo updates
#' their remote location. If the local file is older than the previous day @ 6pm,
#' then do a remote update.
#'
#' @param ticker string format
#' @param yahoo_stock_prices_dir string, directory path 
#' @param from_date string, date %Y:%M:%D
#'
#' @return xts object
#' @export
#'
#' @examples
getYahooDaily_xts<-function(ticker, yahoo_stock_prices_dir, from_date = "2007-01-01"){
  path_to_file<-paste(yahoo_stock_prices_dir, ticker, ".csv", sep = "")
  if (file.exists(path_to_file)){
    last_modified<-file.info(path_to_file)$mtime
    cut_off_time<-strptime("18:00:00", "%H:%M:%S")
    tDiff<-difftime(last_modified, as.POSIXct(cut_off_time))
    if (difftime(last_modified, as.POSIXct(cut_off_time)) > as.difftime(-24, units = "hours")){
      message(paste(ticker, ".csv file is up to date, pulling local...", sep = ""))
      yahoo_daily_xts<-as.xts(read.zoo(path_to_file, header = TRUE, sep = ",", 
                                       colClasses = c("POSIXct", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))
      to_date<-index(yahoo_daily_xts[nrow(yahoo_daily_xts)])
      subset_yahoo_xts(ticker, from = from_date, to = to_date, xts_obj = yahoo_daily_xts)
      return (get(ticker))
    } else {
      message(paste(ticker, ".csv file not up to date, going remote...", sep = ""))
      yahoo_Main_retrieve_and_write(ticker, yahoo_stock_prices_dir, start_date = from_date, end_date = Sys.Date(), write_file = TRUE )
      return (get(ticker))
    }
  } else {
    message(paste(ticker, ".csv does not exist locally, going remote...", sep = ""))
    yahoo_Main_retrieve_and_write(ticker, yahoo_stock_prices_dir, start_date = from_date, end_date = Sys.Date(), write_file = TRUE)
    return (get(ticker))
  }
}

#' subset_yahoo_xts
#' 
#' Subset the xts object passed in or represented by the ticker
#' based on from and to dates.
#'
#' @param ticker 
#' @param from 
#' @param to 
#' @param xts_obj 
#'
#' @return
#' @export
#'
#' @examples
subset_yahoo_xts<-function(ticker, from, to, xts_obj = NULL){
  if (is.null(xts_obj))
    the_xts<-get(ticker)
  else
    the_xts<-xts_obj
  fromTo<-paste(from, to, sep = "/")
  the_xts<-the_xts[fromTo]
  do.call("<<-", list(ticker, the_xts))
}

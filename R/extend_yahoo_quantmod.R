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
check_Rcompat_ticker_names <- function(ticker, path_to_ticker_dir, start_date="2007-01-01", end_date = Sys.Date(), do_write = FALSE) {
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
    if (do_write){
      write_ticker_csv(m, path_to_ticker_dir, column_names = NULL)
      #write_ticker_csv(m, path_to_ticker_dir, column_names=vec_new_col_nm)
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
    if (do_write){
      write_ticker_csv(m, path_to_ticker_dir, column_names = NULL)
      #write_ticker_csv(m, path_to_ticker_dir, column_names = vec_new_col_nm)
    }
  }
  ## adding this on 5/20/2018 while putting together Batch MC Simulation
  ticker_confused_with_px_type <- c('OPEN', 'HIGH', 'LOW', 'CLOSE', 'VOLUME')
  if (ticker %in% ticker_confused_with_px_type){
    message("ticker can be confused with px type...encoding ticker...")
    #m <- paste(ticker, "symbol", sep="")
    m<-ticker
    do.call("<<-", list(m, get(getSymbols(ticker, from =start_date, to=end_date))))
    col_names<-colnames(get(m))
    vec_new_col_nm<-c()
    for (name in col_names){
      name_tokens<-stringr::str_split(name, "\\.")
      new_col_name <- paste("LOU", "symbol.", unlist(name_tokens)[2], sep = "")
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    xts_obj<-get(m)
    colnames(xts_obj) <- make.names(names=vec_new_col_nm)
    do.call("<<-", list(m, xts_obj))
    #message(paste("new col names ", vec_new_col_nm, sep = ""))
    changed_ticker<-ticker
    adjustOHLC_wrapper(m)
    if (do_write){
      #write_ticker_csv(m, path_to_ticker_dir, column_names = NULL)
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
    tf<-str_detect(string=ticker, pattern=fixed("."))
    message(paste("getting dividends for ticker", ticker, sep = " "))
    if (ticker == "BRK.B"){
      col_names<-c(colnames(get(ticker)), paste("BRK-B", "AdjDiff", sep = "."))
      dividends <- getDividends("BRK-B")
    }
    else if (str_detect(string=ticker, pattern=fixed("."))){
      col_names<-c(colnames(get(ticker)), paste(ticker, "AdjDiff", sep = "."))
      retrieved_ticker<-unlist(str_split(ticker, pattern = fixed(".")))[1]
      dividends<-getDividends(retrieved_ticker)
    }
    else if (str_detect(string=ticker, pattern=fixed("symbol"))){
      message("extract ticker from ticker<symbol>...")
      col_names<-c(colnames(get(ticker)), paste(ticker, "AdjDiff", sep = "."))
      retrieved_ticker<-unlist(str_split(ticker, patter = fixed("symbol")))[1]
      message(paste("retrieved ticker is ", retrieved_ticker, sep = ""))
      dividends<-getDividends(retrieved_ticker)
    }
    else {
      col_names<-c(colnames(get(ticker)), paste(ticker, "AdjDiff", sep = "."))
      dividends<-getDividends(ticker)
    }
    message(paste("pulling the object from this ticker ", ticker, sep = ""))
    message(colnames(get(ticker)))
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
    message(paste("the var col_names is ", col_names, sep = ""))
    names(adjusted_data) <- col_names
    do.call("<<-", list(ticker, adjusted_data))
    message("finished with adjustOHLC_wrapper...")
  }
}

adjustOHLC_yahoo<-function(ticker, xts_obj, columnNames = NULL){
  if (!is.null(columnNames)){
    message(paste("columnNames: ", columnNames, sep=""))
    names(xts_obj) <- make.names(columnNames)
  } else {
    columnNames <- c(names(xts_obj))
  }
  dividends<-getDividends(ticker)
  ratios<-adjRatios(dividends = dividends, close = Cl(xts_obj))
  cl_price<-Cl(xts_obj)
  op_price<-Op(xts_obj)
  hi_price<-Hi(xts_obj)
  lo_price<-Lo(xts_obj)
  ad_price<-Ad(xts_obj)
  adjusted_cl<-cl_price*ratios[,"Div"]
  adjusted_op<-op_price*ratios[,"Div"]
  adjusted_hi<-hi_price*ratios[,"Div"]
  adjusted_lo<-lo_price*ratios[,"Div"]
  sanity_check<-(adjusted_cl/ad_price - 1)*100.0
  adjusted_data<-cbind(adjusted_op, adjusted_hi, adjusted_lo, adjusted_cl, Vo(xts_obj), Ad(xts_obj), sanity_check)
  adjusted_data<-na.omit(adjusted_data)
  col_name_prefix<-unlist(str_split(columnNames[1], pattern = fixed(".")))[1]
  columnNames<-c(columnNames, paste(col_name_prefix, 'AdjDiff', sep = "."))
  names(adjusted_data)<-make.names(columnNames)
  return(adjusted_data)
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
yahoo_Main_retrieve_and_write<-function(tickers, yahoo_stock_prices_dir, start_date="2007-01-01", end_date = Sys.Date(), do_write = TRUE){
  path_to_ticker_dir <- yahoo_stock_prices_dir
  list_of_changed_tickers<-sapply(tickers, FUN=check_Rcompat_ticker_names, path_to_ticker_dir, start_date=start_date, end_date = end_date, do_write=do_write)
  list_of_changed_tickers<-list_of_changed_tickers[!is.na(list_of_changed_tickers)]
  tickers<-setdiff(tickers, list_of_changed_tickers)
  for (ticker in tickers){do.call("<<-", list(ticker, get(getSymbols(ticker, from=start_date))))}
  adjustOHLC_wrapper(tickers)
  lapply(ticker, subset_yahoo_xts, from = start_date, to = end_date)
  if (do_write) {
    lapply(tickers, FUN=write_ticker_csv, path_to_ticker_dir = path_to_ticker_dir)
  }
  ## there is a known bug in here, we do not include in this list the tickers that had to be changed due to R incompaitibility
  ## GM - 2/6/2018
  return(tickers)
}

getHistoricalData_yahoo<-function(ticker, yahoo_stock_prices_dir, start_date = "2007-01-01", end_date = Sys.Date(), do_write = TRUE){
  path_to_ticker_dir<-yahoo_stock_prices_dir
  reserved_R_keywords <- c('C', 'T', 'F', 'NA', 'NULL')
  reserved_pxType_keywords <- c("OPEN", "HIGH", "LOW", "CLOSE", "VOLUME", "ADJUSTED")
  #shitty hack.
  if (ticker == "BRK.B"){
    ticker <- "BRK-B"
  }
  if (ticker == "CCL.U"){
    ticker <- "CCL"
  }
  if (ticker == "BF.B"){
    ticker <- "BF-B"
  }
  if(ticker %in% reserved_R_keywords){
    modified_ticker <- paste(ticker, ".", ticker, sep='')
    xts_obj<-getSymbols(ticker, from=start_date, to=end_date, auto.assign = FALSE)
    col_names<-colnames(xts_obj)
    vec_new_col_nm<-c()
    for (name in col_names){
      new_col_name <- paste(ticker, ".", name, sep='')
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    #vec_new_col_nm<-c(vec_new_col_nm, paste(modified_ticker, 'AdjDiff', sep = "."))
    xts_obj<-adjustOHLC_yahoo(ticker, xts_obj, columnNames = vec_new_col_nm)
    if (do_write){
      write_ticker_csv(modified_ticker, path_to_ticker_dir, xts_obj = xts_obj, column_names = NULL)
    }
  } else if (ticker %in% reserved_pxType_keywords) {
    # we will use the ananym of the ticker
    ticker_ananym<-strsplit(ticker, NULL)[[1]] %>% rev() %>% paste(., collapse = '')
    ticker_ananym<-paste(ticker_ananym, "ananym", sep = "")
    xts_obj<-getSymbols(ticker, from=start_date, to=end_date, auto.assign = FALSE)
    col_names<-colnames(xts_obj)
    vec_new_col_nm<-c()
    for (name in col_names){
      px_type<-unlist(str_split(name, pattern = fixed(".")))[2]
      new_col_name <- paste(ticker_ananym, px_type, sep = ".")
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    xts_obj<-adjustOHLC_yahoo(ticker, xts_obj, columnNames = vec_new_col_nm)
    if (do_write){
      write_ticker_csv(ticker, path_to_ticker_dir, xts_obj = xts_obj, column_names = NULL)
    }
  } else if (str_detect(string=ticker, pattern=fixed("-"))) {
    m <- str_replace(string=ticker, pattern=fixed("-"), replacement="_")
    xts_obj<-getSymbols(ticker, from=start_date, to=end_date, auto.assign = FALSE)
    col_names<-colnames(xts_obj)
    vec_new_col_nm<-c()
    for(name in col_names){
      new_col_name <-str_replace(string=name, pattern=fixed("-"), replacement="_")
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    xts_obj<-adjustOHLC_yahoo(ticker, xts_obj, columnNames = vec_new_col_nm)
    if (do_write){
      write_ticker_csv(ticker, path_to_ticker_dir, xts_obj = xts_obj, column_names = NULL)
    }
  } else {
    xts_obj<-getSymbols(ticker, from=start_date, to=end_date, auto.assign = FALSE)
    xts_obj<-adjustOHLC_yahoo(ticker, xts_obj)
    if (do_write){
      write_ticker_csv(ticker, path_to_ticker_dir, xts_obj = xts_obj, column_names = NULL)
    }
  }
  return (xts_obj)
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
getYahooDaily_xts<-function(ticker, yahoo_stock_prices_dir, from_date = "2007-01-01", return_format = "xts"){
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
      yahoo_daily_xts<-subset_yahoo_xts(ticker, from = from_date, to = to_date, xts_obj = yahoo_daily_xts)
      if (return_format == "tibble"){
        the_tibble<-convertYahoo_xts2tibble(ticker, yahoo_daily_xts)
        return (the_tibble)
      }
      return (yahoo_daily_xts)
    } else {
      message(paste(ticker, ".csv file not up to date, going remote...", sep = ""))
      yahoo_daily_xts<-getHistoricalData_yahoo(ticker, yahoo_stock_prices_dir = yahoo_stock_prices_dir, start_date=from_date, end_date = Sys.Date(), do_write = TRUE)
      #yahoo_Main_retrieve_and_write("BRK-B", yahoo_stock_prices_dir, start_date = from_date, end_date = Sys.Date(), do_write = TRUE)
      if (return_format == "tibble"){
        the_tibble <- convertYahoo_xts2tibble(ticker, xts_obj = yahoo_daily_xts)
        return (the_tibble)
      }
      return (yahoo_daily_xts)
    }
  } else {
    message(paste(ticker, ".csv does not exist locally, going remote...", sep = ""))
    yahoo_daily_xts<-getHistoricalData_yahoo(ticker, yahoo_stock_prices_dir = yahoo_stock_prices_dir, start_date = from_date, end_date = Sys.Date(), do_write = TRUE)
    #yahoo_Main_retrieve_and_write("BRK-B", yahoo_stock_prices_dir, start_date = from_date, end_date = Sys.Date(), do_write = TRUE)
    if (return_format == "tibble"){
      the_tibble<-convertYahoo_xts2tibble(ticker, xts_obj = yahoo_daily_xts)
      return (the_tibble)
    }
    return (yahoo_daily_xts)
  }
}

#' convertYahoo_xts2tibble
#' 
#' Convert xts to tibble.
#'
#' @param ticker 
#' @param xts_obj 
#'
#' @return
#' @export
#'
#' @examples
convertYahoo_xts2tibble<-function(ticker, xts_obj = NULL){
  if (is.null(xts_obj))
    the_xts<-get(ticker)
  else
    the_xts<-xts_obj
  the_tibble<-the_xts %>% as_tibble() %>% rownames_to_column(var = "Date") %>% mutate(Date = ymd(Date))
  return (the_tibble)
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
  return (the_xts)
  
}

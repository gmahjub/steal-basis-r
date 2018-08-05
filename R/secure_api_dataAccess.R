#' get_api_key
#'
#' Returns the api key for accessing remote data vendors.
#'
#' @param api_key_file path to local file where api keys are stored. Must have
#'   columns labeled "VENDOR" and "API_KEY". Should be a csv file.
#' @param vendor options currently are "ALPHAVANTAGE", and "QUANDL".
#'
#' @return an api key as character vector for the specified vendor.
#' @export
#'
#' @examples
get_api_key<-function(api_key_file, vendor = "ALPHAVANTAGE"){
  #message(paste("api key file: ", api_key_file, sep = ""))
  #message(paste("vendor: ", vendor, sep = ""))
  api_keys_tibble<-as.tibble(read.csv(api_key_file, header = TRUE))
  api_key<- api_keys_tibble %>% filter(Vendor == vendor) %>% select(API_KEY)
  return(as.character(api_key$API_KEY))
}

#' set_alphavantage_api_key
#'
#' calls the av_api_key() method from the alphavantage library to set the key in
#' order to pull data from alphavantage
#'
#' @return nothing
#' @export
#'
#' @examples
set_alphavantage_api_key<-function(api_key_file){
  av_api_key(get_api_key(api_key_file, vendor = "ALPHAVANTAGE"))
}

#' set_quandl_api_key
#'
#' sets the quandl api key within the quandl library api so that data can be
#' pulled from the quandl remote repository.
#'
#' @return nothing
#' @export
#'
#' @examples
set_quandl_api_key<-function(api_key_file){
  quandl_api_key(get_api_key(api_key_file, vendor = "QUANDL"))
}

#' set_riingo_api_key
#' 
#' sets the riingo api key within the riingo library api so that data can be
#' pulled form the riingo remote repository.
#'
#' @param api_key_file 
#'
#' @return
#' @export
#'
#' @examples
set_riingo_api_key<-function(api_key_file){
  riingo_set_token(get_api_key(api_key_file, vendor = "TIINGO"))
}

#' get_intraday_data_alphavantager
#'
#' Calls the av_get method of the alphavantage api and specifies the intraday
#' time series, where the options for the interval are listed below. This
#' function is part of the tibble functionality and does not use xts, but the
#' returned object can be coerced to an xts object.
#'
#' @param ticker ticker as string
#' @param interval options are 1min, 5min, 15min, 30min, 60min
#'
#' @return a tibble object containing the alphavantage tick pull data, as
#'   formatted by alphavantage. See the documentation at alphavantage.co for
#'   more info.
#' @export
#'
#' @examples
get_intraday_data_alphavantager<-function(ticker, error_log, interval="1min", outputsize = "full"){
  set_alphavantage_api_key(path_to_api_key_file)
  tibble_obj<-tryCatch(
    av_get(symbol=ticker, av_fun="TIME_SERIES_INTRADAY", interval=interval, outputsize=outputsize),
    error = function(e) { message("there was an error, writing ticker to error log..."); write_error_log(ticker, error_log); Sys.sleep(5)})
  return(tibble_obj)
  # use message() function instead of the print() function across the board
  # use stop() to stop exection if the error was a fatal one
  # use warning() to if error is not fatal, do not want to sop execution.
}

#' write_error_log
#'
#' @param ticker 
#' @param error_file_dir 
#'
#' @return
#' @export
#'
#' @examples
write_error_log<-function(ticker, error_log, message = NA){
  message(paste("In write_error_log, writing ", error_log, sep = ""))
  log_con <- file(error_log, open = "a")
  if (is.na(message)){
    error_message<-paste(ticker, "Failed,Failed\n", sep = ",")
  } else {
    error_message<-paste(ticker, message, "Failed\n", sep = ",")
  }
  cat(error_message, file = log_con)
  flush(log_con)
  close(log_con)
}

#' get_intraday_data_alphavantager_no_exception_handling
#' 
#' Delete this function, we should never refuse to use exception handling.
#' Using this funciton only for test, never prod.
#'
#' @param ticker 
#' @param interval 
#' @param outputsize 
#'
#' @return
#' @export
#'
#' @examples
get_intraday_data_alphavantager_no_exception_handling<-function(ticker, interval="1min", outputsize = "full"){
  tibble_obj<-av_get(symbol = ticker, av_fun = "TIME_SERIES_INTRADAY", interval = interval, outputsize = outputsize)
  return(tibble_obj)
}

#' eod_batch_IBKR_helper
#'
#' @param ticker 
#' @param path_to_ticker_dir 
#' @param error_log 
#'
#' @return
#' @export
#'
#' @examples
eod_batch_IBKR_helper<-function(ticker, path_to_ticker_dir, error_log, asset_class, IBKR_px_type = NA, port_number = 7496, force_noSplit = FALSE){
  require(lubridate)
  message(paste("begin intra minutely tick pull", ticker, sep=" "))
  message(paste("connect to IB API port number ", port_number, sep = ""))
  file_path <-paste(path_to_ticker_dir, ticker, ".csv", sep = "")
  remote<-FALSE
  if (file.exists(file_path)){
    message(paste("file does exist...", path_to_ticker_dir, sep = ""))
    if (is.na(IBKR_px_type)) {
      local_intraday_tibble<-as.tibble(read.csv(file = file_path, header = TRUE, sep=',', nrows=-1,
                                                colClasses = c("POSIXct", "numeric", "numeric", "numeric", "numeric", "integer",
                                                              "numeric", "integer", "integer")))
    } else if (IBKR_px_type == "ADJUSTED_LAST") {
      local_intraday_tibble<-as.tibble(read.csv(file = file_path, header = TRUE, sep = ',', nrows = -1,
                                                colClasses = c("Date", "numeric", "numeric", "numeric", "numeric", "integer", 
                                                               "numeric", "integer", "integer")))
    }
    local_intraday_tibble<-local_intraday_tibble[,1:ncol(local_intraday_tibble)]
    last_row_of_local<-local_intraday_tibble[nrow(local_intraday_tibble),]
    last_timestamp_local<-last_row_of_local$BarTimeStamp
    if (IBKR_px_type == "ADJUSTED_LAST"){
      when_was_mkt_open_last_value<-date(when_was_mkt_open_last())
    }
    else if (is.na(IBKR_px_type)){
      when_was_mkt_open_last_value<-when_was_mkt_open_last()
    }
    save_original_tz <- last_timestamp_local
    remote_intraday_tibble<-FALSE
    message(paste("market was last open ", when_was_mkt_open_last_value))
    message(paste("last timestamp local ", last_timestamp_local))
    if (difftime(last_timestamp_local, when_was_mkt_open_last_value, tz="UTC", units = c("mins")) < -1) {
      message(paste("file exists, but not up to date, going remote...", ticker, sep = ""))
      if (toupper(asset_class) == FUTURE_ASSET_CLASS){
        remote_intraday_tibble <- getHistoricalData_futs(ticker, error_log_file = error_log, port_number = port_number)
      } else if (toupper(asset_class) == FOREX_ASSET_CLASS){
        remote_intraday_tibble <- getHistoricalData_forex(ticker, error_log_file = error_log, port_number = port_number)
      } else if (toupper(asset_class) == EQUITY_ASSET_CLASS){
        if (IBKR_px_type == "ADJUSTED_LAST"){
          # we need to calculate how many days are missing from the local tibble and pull that many days remotely.
          num_days_local_missing<-difftime(last_timestamp_local, date(when_was_mkt_open_last()), tz = "UTC", units = c("days"))
          message(paste("number of days missing from local is ", num_days_local_missing, sep = ""))
          if (num_days_local_missing < 0){
            duration_string<-paste(num_days_local_missing*-1, "D", sep = " ")
            message(paste("duration string is ", duration_string, sep = ""))
            remote_intraday_tibble<-getHistoricalData(ticker, barSize = "1 day", duration = duration_string, 
                                                      whatToShow = IBKR_px_type, write_out = FALSE, 
                                                      path_to_ticker_dir = path_to_ticker_dir, error_log_file = error_log, 
                                                      port_number = 4001, end_date_time = "")
          } else {
            message(paste("local file is up to date...", file_path, sep = ""))
          }
        } else {
          remote_intraday_tibble <- getHistoricalData(ticker, error_log_file = error_log, port_number = port_number)
        }
      }
      if (!is.null(remote_intraday_tibble)){
        remote_intraday_tibble <- tk_tbl(remote_intraday_tibble, rename_index = "BarTimeStamp")
        remote<-TRUE
        if (!is.null(remote_intraday_tibble)){
          message(save_original_tz)
          new_timeseries_to_append<-remote_intraday_tibble %>% filter(BarTimeStamp > save_original_tz)
          intraday_tibble_obj<-rbind(local_intraday_tibble, new_timeseries_to_append)
          intraday_tibble_obj<-tk_xts(intraday_tibble_obj, date_var = BarTimeStamp, select = -BarTimeStamp)
          do.call("<-", list(paste(ticker, "intra", sep='.'), intraday_tibble_obj))
          write_intraday_IBKR(ticker, get(paste(ticker, "intra", sep='.')), path_to_ticker_dir = path_to_ticker_dir, intraday = TRUE)
        }
      }
    } else {
      message(paste("local file is up to date...", path_to_ticker_dir, sep = ""))
    }
  } else {
    # means we do not have any data locally for this ticker
    message(paste("no existing file, going remote...", ticker, sep = ""))
    remote<-TRUE
    if (toupper(asset_class) == FUTURE_ASSET_CLASS){
      remote_intraday_tibble<-split_IBKR_histData_req(ticker, asset_class = FUTURE_ASSET_CLASS, error_log = error_log, port_number = port_number, useRTH = 0, 
                                                      whatToShow = "TRADES")
    } else if (toupper(asset_class) == FOREX_ASSET_CLASS){
      remote_intraday_tibble<-split_IBKR_histData_req(ticker, asset_class = FOREX_ASSET_CLASS, error_log = error_log, port_number = port_number, useRTH = 1, 
                                                      whatToShow = "MIDPOINT")
    } else if (toupper(asset_class) == EQUITY_ASSET_CLASS) {
      if (IBKR_px_type == "ADJUSTED_LAST"){
        remote_intraday_tibble<-getHistoricalData(ticker, barSize = "1 day", duration = '20 Y', whatToShow = IBKR_px_type, write_out = FALSE, path_to_ticker_dir = path_to_ticker_dir, 
                                                  error_log_file = error_log, port_number = 4001, end_date_time = "")
      } else {
        remote_intraday_tibble<-split_IBKR_histData_req(ticker, asset_class = EQUITY_ASSET_CLASS, error_log = error_log, port_number = port_number, useRTH = 1, 
                                                        whatToShow = "TRADES", hist_len = 12, hist_time_unit = 'months')
      }
    }
    if (!is.null(remote_intraday_tibble)){
      do.call("<-", list(paste(ticker, "intra", sep='.'), remote_intraday_tibble))
      write_intraday_IBKR(ticker, get(paste(ticker, "intra", sep = ".")), path_to_ticker_dir = path_to_ticker_dir, intraday = TRUE)
    }
  }
  # lets do some clean up if possible
  rm(remote_intraday_tibble)
  if (remote){
    Sys.sleep(5)
    remote<-FALSE
  }
}

#' eod_batch_av_helper
#'
#' Appends new intraday data to data csv files on local respository. Downloads
#' new timeseries and appends. If a file does not exist on local drive,
#' downloads full outputsize and writes that out to file.
#'
#' HARDCODES parameters!!!
#'
#' @param ticker
#' @param path_to_ticker_dir
#'
#' @return nothing
#' @export
#'
#' @examples
eod_batch_av_helper<-function(ticker, path_to_ticker_dir, error_log){
  require(lubridate)
  message(paste("begin intra minutely tick pull", ticker, sep=" "))
  file_path <- paste(path_to_ticker_dir, ticker, ".csv", sep="")
  remote<-FALSE
  if (file.exists(file_path)){
    message(paste("file does exist...", path_to_ticker_dir, sep = ""))
    local_intraday_tibble<-as.tibble(read.csv(file=file_path, header=TRUE, sep=',', nrows=-1,
                                              colClasses = c("character", "POSIXct", "numeric",
                                                             "numeric", "numeric", "numeric", "integer")))
    local_intraday_tibble<-local_intraday_tibble[,2:ncol(local_intraday_tibble)]
    local_intraday_tibble<-local_intraday_tibble %>% setNames(., c("BarTimeStamp", "Open", "High", "Low", "Close", "Volume"))
    last_row_of_local<-local_intraday_tibble[nrow(local_intraday_tibble),]
    last_timestamp_local<-last_row_of_local$BarTimeStamp
    last_timestamp_local<-as.POSIXct(last_timestamp_local, tz = "America/New_York")
    local_intraday_tibble$BarTimeStamp<-force_tz(local_intraday_tibble$BarTimeStamp, tzone = "America/New_York")
    save_original_tz<-as.POSIXct(last_timestamp_local)
    attr(last_timestamp_local, "tzone")<-"UTC"
    remote_intraday_tibble<-FALSE
    if (difftime(last_timestamp_local, when_was_mkt_open_last(), tz="UTC", units = c("mins")) < 0) {
      message(paste("file exists, but not up to date, going remote...", ticker, sep = ""))
      remote_intraday_tibble <- get_intraday_data_alphavantager(ticker, error_log, interval = "1min", outputsize = "full")
      remote<-TRUE
      if (!is.null(remote_intraday_tibble)){
        remote_intraday_tibble<-remote_intraday_tibble %>% setNames(c("BarTimeStamp", "Open", "High", "Low", "Close", "Volume"))
        remote_intraday_tibble$BarTimeStamp<-force_tz(remote_intraday_tibble$BarTimeStamp, tzone = "America/New_York")
        new_timeseries_to_append<-remote_intraday_tibble %>% filter(BarTimeStamp > save_original_tz)
        intraday_tibble_obj<-rbind(local_intraday_tibble, new_timeseries_to_append)
        do.call("<-", list(paste(ticker, "intra", sep='.'), intraday_tibble_obj))
        write_intraday_av(ticker, get(paste(ticker, "intra", sep='.')), path_to_ticker_dir = path_to_ticker_dir,
                          column_names = c("BarTimeStamp", "Open", "High", "Low", "Close", "Volume") )
      }
    } else {
      message(paste("local file is up to date...", path_to_ticker_dir, sep = ""))
    }
  } else {
    # means we do not have any data locally for this ticker
    message(paste("no existing file, going remote...", ticker, sep = ""))
    remote<-TRUE
    remote_intraday_tibble <- get_intraday_data_alphavantager(ticker, error_log, interval = "1min", outputsize = "full")
    do.call("<-", list(paste(ticker, "intra", sep='.'), remote_intraday_tibble))
    custom_headers<-c("BarTimeStamp", "Open", "High", "Low", "Close", "Volume")
    write_intraday_av(ticker, get(paste(ticker, "intra", sep='.')), path_to_ticker_dir = path_to_ticker_dir, column_names = custom_headers)
  }
  # lets do some clean up if possible
  rm(remote_intraday_tibble)
  if (remote){
    Sys.sleep(5)
    remote<-FALSE
  }
}

#' subset_av_tibble
#'
#' @param cut_off_date_as_char cut-off date
#' @param path_to_ticker_dir path to directory location of ticks file
#' @param file_name the name of the ticks file, usually <something>.csv
#' @param column_headers the names of the column headers for the alphavantage
#'   ticker file, in a vector. Defaults to c("BarTimeStamp", "Open", "High", "Low", "Close")
#'
#' @return subsetted tibble, contains all the data up to cut off date input
#'   parameter
#' @export
#'
#' @examples
subset_av_tibble<-function(cut_off_date_as_char, path_to_ticker_dir, file_name, column_headers = c("BarTimeStamp", "Open", "High", "Low", "Close", "Volume")){
  cut_off_date<-as.POSIXct(cut_off_date_as_char)
  file_path <- paste(path_to_ticker_dir, file_name, sep = "")
  local_intraday_tibble<-as.tibble(read.csv(file=file_path, header=TRUE, sep=',', nrows=-1))
  local_intraday_tibble<-local_intraday_tibble[,2:ncol(local_intraday_tibble)]
  local_intraday_tibble<-local_intraday_tibble %>% setNames(., column_headers)
  local_intraday_tibble$BarTimeStamp<-as.POSIXct(local_intraday_tibble$BarTimeStamp)
  local_intraday_tibble<-local_intraday_tibble %>% filter(BarTimeStamp < cut_off_date)
  return (local_intraday_tibble)
}

#' get_files_in_dir
#'
#' Simply a wrapper for list.files. Defaults to get all the csv files in a the
#' directory, path_to_ticker_dir.
#'
#' @param path_to_ticker_dir
#' @param pattern a pattern to match, e.g "*.csv".
#'
#' @return the list of files matching the patter in the directory,
#'   path_to_ticker_dir
#' @export
#'
#' @examples
get_files_in_dir<-function(path_to_ticker_dir, pattern = "*.csv"){
  file_list<-list.files(path_to_ticker_dir, pattern = pattern)
  return(file_list)
}

#' when_was_mkt_open_last
#'
#' Returns the last time the New York Equities market was open. Currently only
#' supports New York equity markets, but will expand in the future. The last
#' time the market was open is actually the last time the market closed, we
#' should change the name. Accounts for dst differences between UTC and EST/EDT.
#' This function is a work in progress, but is functional right now for NY
#' Equities
#'
#' @return a POSIXct object, UTC timezone, of when NY equities was last open
#'   (the last time the market closed)
#' @export
#'
#' @examples
when_was_mkt_open_last<-function(){
  require(lubridate)
  if (dst(Sys.time())){ nyse_exchange_mkt_close_utc<-c("20:00:00"); nyse_exchange_mkt_open_utc<-c("13:30:00") }
  else { nyse_exchange_mkt_close_utc<-c("21:00:00"); nyse_exchange_mkt_open_utc<-c("14:30:00") }
  posixct_nyse_exchange_mkt_open<-as.POSIXct(nyse_exchange_mkt_open_utc, format="%H:%M:%S", tz = "UTC")
  posixct_nyse_exchange_mkt_close<-as.POSIXct(nyse_exchange_mkt_close_utc, format="%H:%M:%S", tz = "UTC")
  current_time_utc<-as.POSIXct(Sys.time(), tz = Sys.timezone())
  attr(current_time_utc, "tzone") <- "UTC"
  current_hour_utc<-unclass(as.POSIXlt(current_time_utc))$hour
  current_min_utc<-unclass(as.POSIXlt(current_time_utc))$min
  current_day_utc<-unclass(as.POSIXlt(current_time_utc))$mday
  current_year_utc<-unclass(as.POSIXlt(current_time_utc))$year + 1900
  current_month_utc<-unclass(as.POSIXlt(current_time_utc))$mon + 1
  current_day_of_week_utc<-unclass(as.POSIXlt(current_time_utc))$wday
  current_hms<-hms(paste(current_hour_utc, current_min_utc, "0", sep = " "))
  nyse_exchange_close_hms<-hms(nyse_exchange_mkt_close_utc)
  nyse_exchange_open_hms<-hms(nyse_exchange_mkt_open_utc)
  last_datetime_ymd_hms <- NA
  if (current_day_of_week_utc == 0){
    # its currently Sunday, the last time market was open was 2 days ago, Friday.
    last_datetime_ymd_hms <- ymd_hms(paste(current_year_utc, current_month_utc, current_day_utc, nyse_exchange_mkt_close_utc, sep = " ")) - days(2)
  } else if (current_day_of_week_utc == 6){
    # its currently Saturday, the last time the market was open was yesterday.
    last_datetime_ymd_hms <- ymd_hms(paste(current_year_utc, current_month_utc, current_day_utc, nyse_exchange_mkt_close_utc, sep = " ")) - days(1)
  } else {
    if (current_hms < nyse_exchange_close_hms){
      if (current_hms < nyse_exchange_open_hms){
        # the last time the market was open was the previous day close
        last_datetime_ymd_hms <- ymd_hms(paste(current_year_utc, current_month_utc, current_day_utc, nyse_exchange_mkt_close_utc, sep = " ")) - days(1)
        if (as.POSIXlt(last_datetime_ymd_hms)$wday == 0){
          # if its monday, and yesterday was Sunday, then correct it.
          last_datetime_ymd_hms <- ymd_hms(paste(current_year_utc, current_month_utc, current_day_utc, nyse_exchange_mkt_close_utc, sep = " ")) - days(3)
        }
      } else {
        # then we are in the middle of trading hours, market is open
        last_datetime_ymd_hms <- ymd_hms(current_time_utc)
      }
    } else if (current_hms > nyse_exchange_close_hms){
      # then we are still on the same day, but after hours
      last_datetime_ymd_hms<-ymd_hms(as.POSIXct(nyse_exchange_mkt_close_utc, format="%H:%M:%S", tz = "UTC"))
    }
  }
  return (last_datetime_ymd_hms)
}

#' split_IBKR_histData_req
#'
#' @param symbol 
#' @param asset_class 
#' @param currency 
#' @param port_number 
#' @param useRTH 
#' @param whatToShow 
#' @param hist_len_days 
#'
#' @return
#' @export
#'
#' @examples
split_IBKR_histData_req<-function(symbol, asset_class, error_log, currency = "USD", port_number = 4001, useRTH = 1, whatToShow = "TRADES", 
                                  hist_len = 365, hist_time_unit = 'days'){
  twsConnection <- connect2TWS(port_number = port_number)
  if (toupper(asset_class) == FUTURE_ASSET_CLASS) {
    down_counter<-seq(hist_len-1, 1, -1)
    contract_obj<-getFutContractObject(symbol = symbol)
    contract_obj$sectype<-"CONTFUT"
  } else if (toupper(asset_class) == FOREX_ASSET_CLASS) {
    down_counter<-seq(hist_len-1, 1, -1)
    contract_obj<-getFxContractObject(symbol = symbol, currency = currency)
  } else if (toupper(asset_class) == EQUITY_ASSET_CLASS) {
    down_counter<-seq(hist_len-3, 0, -3)
    contract_obj<-twsEquity(symbol = symbol)
  }
  currentTime<-reqCurrentTime(twsConnection)
  disconnectTWSConn(twsConnection = twsConnection)
  currentTime<-as.POSIXct(as.POSIXlt(currentTime))
  list_of_xts<-lapply(down_counter, FUN = split_helper, y = currentTime, contract_obj = contract_obj, useRTH = useRTH, whatToShow = whatToShow,
                      asset_class = asset_class, error_log = error_log, port_number = port_number, hist_time_unit = hist_time_unit)
  result_xts<-do.call(rbind, list_of_xts)
  result_xts<-result_xts[ ! duplicated(index(result_xts), fromLast = TRUE), ]
  #disconnectTWSConn(twsConnection = twsConnection)
  return (result_xts)
}

#' split_helper (private)
#'
#' @param x 
#' @param y 
#' @param tws_conn_obj 
#' @param contract_obj 
#' @param useRTH 
#' @param whatToShow 
#' @param asset_class 
#'
#' @return
#' @export
#'
#' @examples
split_helper<-function(x, y, contract_obj, useRTH, whatToShow, asset_class, error_log, port_number, hist_time_unit){
  # private function
  if (hist_time_unit == 'days') {
    end_date_time<- (y-days(x))
  } else if (hist_time_unit == 'months') {
    end_date_time<-(y-months(x))
  }
  end_date_time<-as.character(end_date_time, format="%Y%m%d %H:%M:%S")
  message(paste("remaining time in tick pull...", x, hist_time_unit, sep = " "))
  if (toupper(asset_class) == FUTURE_ASSET_CLASS) {
    single_day_hist_xts<-getHistoricalData_futs(contract_obj$symbol, barSize = "1 min", duration = "1 D", whatToShow = whatToShow, secType = "CONTFUT", error_log_file = error_log, 
                           port_number = port_number, end_date_time = end_date_time)
  } else if (toupper(asset_class) == EQUITY_ASSET_CLASS) {
    single_day_hist_xts<-getHistoricalData(contract_obj$symbol, barSize = "1 min", duration = "3 M", whatToShow = whatToShow, error_log_file = error_log, 
                                           port_number = port_number, end_date_time = end_date_time)
  } else if (toupper(asset_class) == FOREX_ASSET_CLASS) {
    single_day_hist_xts<-getHistoricalData_forex(contract_obj$symbol, contract_obj$currency, barSize = "1 min", duration = "1 D", error_log_file = error_log, 
                                                 port_number = port_number, end_date_time = end_date_time)
  }
  Sys.sleep(5)
  return (single_day_hist_xts)
}

#' getSymbolsUniverse
#' 
#' This funciton is used to get a complete list of tickers to pull from the flat files,
#' containing both ETF tickers and the Russell 1000, and Russell 2000.
#'
#' @return list of tickers
#' @export
#'
#' @examples
getSymbolsUniverse<-function(assetClass){
  if (Sys.info()['sysname'] == "Darwin"){
    theFileOfFiles<-paste(getwd(), "../../../data/alphavantageTickPullFiles.csv", sep = "/")
    holdings_file_dir<-paste(getwd(), "../../../data/", sep = "/")
  } else {
    theFileOfFiles<-paste(getwd(), "..\\..\\..\\..\\data\\alphavantageTickPullFiles.csv", sep = "\\")
    holdings_file_dir<-paste(getwd(), "..\\..\\..\\..\\data\\", sep = "\\")
  }
  fileOfFiles<-read.csv(file = theFileOfFiles, header = TRUE, sep = ",")
  fileOfFiles<-fileOfFiles %>% filter(toupper(AssetClass) == toupper(assetClass))
  fileList<-as.vector(fileOfFiles$SymbolsFileName)
  append_dir<-function(f) paste(holdings_file_dir, f, sep = "")
  list_ticker_file_paths<-sapply(fileList, FUN = append_dir)
  if (length(list_ticker_file_paths) == 1){
    single_tibble<-get_holdings_from_file_in_tibble_fmt(list_ticker_file_paths)
    single_filtered_tibble<-get_holdings_filtered(single_tibble, filter_column_name = "Asset.Class", filter_operator = "==", filter_value = "Equity")
    ticker_vector<-getTickersAsVector(single_filtered_tibble)
    list_of_tickers<-as.character(unlist(ticker_vector))
  } else {
    list_of_tibbles<-sapply(list_ticker_file_paths, FUN = get_holdings_from_file_in_tibble_fmt)
    list_of_filtered_tibbles<-sapply(list_of_tibbles, FUN = get_holdings_filtered, filter_column_name = "Asset.Class", 
                                   filter_operator = "==", filter_value = "Equity")
    list_of_ticker_vectors<-sapply(list_of_filtered_tibbles, FUN = getTickersAsVector)
    list_of_tickers<-as.character(unlist(list_of_ticker_vectors))
  }
  return (list_of_tickers)
}

#' eod_batch_av_intraday
#' 
#' Contains error logging and less hardcoding now. Much better. Pulls ticks
#' from alphavanatge source. Defaults to pull symbols from a flat file.
#'
#' @param path_to_ticker_dir 
#' @param path_to_api_key_file 
#' @param list_of_tickers 
#'
#' @return
#' @export
#'
#' @examples
eod_batch_av_intraday <- function(path_to_ticker_dir, path_to_api_key_file, list_of_tickers=NULL){
  set_alphavantage_api_key(path_to_api_key_file)
  error_log_file<-paste(Sys.Date(), "AvTickPull_FailStatus.csv", sep = ".")
  if (Sys.info()['sysname'] == "Darwin"){
    error_file_dir<-paste(getwd(), "../../../data/alphavantage/logs", sep = "/")
    error_log_file<-paste(error_file_dir, error_log_file, sep = "/")
  } else {
    error_file_dir<-paste(getwd(), "..\\..\\..\\..\\data\\alphavantage\\logs", sep = "\\")
    error_log_file<-paste(error_file_dir, error_log_file, sep = "\\")
  }
  if (is.null(list_of_tickers)){
    log_con <- file(error_log_file, open = "a")
    cat("Symbol,Message,Status\n", file = log_con)
    flush(log_con)
    close(log_con)
    list_of_tickers<-getSymbolsUniverse(EQUITY_ASSET_CLASS)
  }
  sapply(list_of_tickers, FUN = eod_batch_av_intraday_error_helper, path_to_ticker_dir = path_to_ticker_dir, error_log = error_log_file)
  # rerun
  failedFile<-read.csv(file = error_log_file, header = TRUE, sep = ",")
  tickerList<-as.character(failedFile$Symbol)
  rerun_log_file<-paste(error_log_file, "1", sep =".")
  log_con<-file(rerun_log_file, open = "a")
  cat("Symbol,Message,Status\n", file = log_con)
  flush(log_con)
  close(log_con)
  Sys.sleep(10)
  sapply(tickerList, FUN = eod_batch_av_intraday_error_helper, path_to_ticker_dir = path_to_ticker_dir, error_log = rerun_log_file)
  # final run
  rerunFile<-read.csv(file = rerun_log_file, header = TRUE, sep = ",")
  tickerList<-as.character(rerunFile$Symbol)
  finalRun_log_file<-paste(error_log_file, "2", sep=".")
  log_con<-file(finalRun_log_file, open = "a")
  cat("Symbol,Status\n", file = log_con)
  flush(log_con)
  close(log_con)
  Sys.sleep(10)
  sapply(tickerList, FUN = eod_batch_av_intraday_error_helper, path_to_ticker_dir = path_to_ticker_dir, error_log = finalRun_log_file)
}

#' eod_batch_IBKR_intraday
#' 
#' we get many different types of data from IBrokers, not just equities. So we need asset_class parameter to identify
#' what we are going to pull, whether its equities, futures, forex, etc...
#'
#' @param path_to_ticker_dir 
#' @param tickers 
#' @param port_number 
#' @param error_log_file_name 
#' @param asset_class valid options are "Equities", "Futures", "FX" - others may be added in the future.
#'
#' @return
#' @export
#'
#' @examples
eod_batch_IBKR_intraday<-function(path_to_ticker_dir, asset_class, port_number, IBKR_px_type = NA, error_log_file_name = "IBKRgetHist_FailStatus.csv", tickers=NULL){
  error_log_file<-paste(Sys.Date(), error_log_file_name, sep = ".")
  message(paste("Error log file is ", error_log_file, sep = ""))
  if (Sys.info()['sysname'] == "Darwin"){
    error_file_dir<-paste(getwd(), "../../../data/IBKR/logs", sep = "/")
    error_log_file<-paste(error_file_dir, error_log_file, sep = "/")
  } else {
    error_file_dir<-paste(getwd(), "..\\..\\..\\..\\data\\IBKR\\logs", sep = "\\")
    error_log_file<-paste(error_file_dir, error_log_file, sep = "\\")
  }
  if (is.null(tickers)){
    log_con<-file(error_log_file, open = "a")
    cat("Symbol,Message,Status\n", file = log_con)
    flush(log_con)
    close(log_con)
    tickers<-getSymbolsUniverse(assetClass = toupper(asset_class))
  }
  if (is.na(IBKR_px_type)){
    sapply(tickers, FUN = eod_batch_IBKR_helper, path_to_ticker_dir = path_to_ticker_dir, error_log = error_log_file, 
           port_number = port_number, asset_class = asset_class)
  } else {
    sapply(tickers, FUN = eod_batch_IBKR_helper, path_to_ticker_dir = path_to_ticker_dir, error_log = error_log_file, 
           IBKR_px_type = IBKR_px_type, port_number = port_number, asset_class = asset_class)
  }
  
}

eod_batch_IBKR_daily<-function(path_to_ticker_dir, asset_class, port_number, error_log_file_name = "IBKRgetDailyHist_FailStatus.csv", tickers=NULL){
  error_log_file<-paste(Sys.Date(), error_log_file_name, sep = ".")
  message(paste("Error log file is ", error_log_file, sep = ""))
  
}

#' getTickersAsVector
#' 
#' Private helper method for returning list of ticker as a vector, single column.
#' We extract to a method because we need a bit more actual functionality around
#' warnings, specifically a warning triggered by not finding a column named "Symbol",
#' and subsequently using column name "Ticker" instead.
#'
#' @param tibble_obj 
#'
#' @return list of tickers.
#' @export
#'
#' @examples
getTickersAsVector<-function(tibble_obj){
  tickers<-tryCatch( as.character(tibble_obj$Symbol), warning = function(w) { as.character(tibble_obj$Ticker) })
  return (tickers)
}

#' eod_batch_av_intraday_error_helper
#'
#' internal function, helps to capture errors when we are doing batch pulls of
#' ticks. Means we can capture the error, and continue on to the next ticker,
#' instad of failing the entire batch.
#'
#' @param ticker
#' @param path_to_ticker_dir
#'
#' @return
#' @export
#'
#' @examples
eod_batch_av_intraday_error_helper<-function(ticker, path_to_ticker_dir, error_log){
  return (tryCatch(eod_batch_av_helper(ticker, path_to_ticker_dir, error_log),
                   error=function(e) message(paste(ticker, e, sep = ":"))))
}

#' get_daily_data_alphavantager
#'
#' Uses the av_get() function of the alphavantage api to pull daily periodicity
#' time series data. Pulls the full data, outputsize set to "full" such that all
#' the data in the remote alphavantage db is pulled.
#'
#' @param ticker ticker as string
#'
#' @return a tibble object containing the time series of the specified ticker.
#' @export
#'
#' @examples
get_daily_data_alphavantager<-function(ticker){
  tibble_obj<-av_get(symbol=ticker, av_fun="TIME_SERIES_DAILY", outputsize="full")
  return(tibble_obj)
}

#' get_daily_adjusted_data_alphavantager
#'
#' Get daily adjusted close data from alphavantage api. Returned is a tibble object.
#'
#' @param ticker ticker as string
#'
#' @return tibble object with the adjusted close time series data in tibble format.
#' @export
#'
#' @examples
get_daily_adjusted_data_alphavantager<-function(ticker){
  tibble_obj<-av_get(symbol=ticker, av_fun="TIME_SERIES_DAILY_ADJUSTED", outputsize="full")
  return(tibble_obj)
}

#' get_daily_adjusted_data_av
#'
#' Using the quantmod api, pull daily adjusted close data from alphavantage
#' repository. Returns an xts object.
#'
#' @param ticker ticker as string
#' @param start_date date to start pulling data, in format YYYY-MM-DD
#' @param av_api_key alphavantage api key, Required.
#'
#' @return xts object with daily periodicity data, starting from start_date
#' @export
#'
#' @examples
get_daily_adjusted_data_av<-function(ticker, av_api_key, start_date){
  symbol <- getSymbols(ticker, src="av", from=start_date, api.key=av_api_key, auto.assign = TRUE, adjusted=TRUE, output.size="full")
}

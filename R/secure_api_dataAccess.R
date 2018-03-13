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
get_intraday_data_alphavantager<-function(ticker, interval="1min", outputsize = "full", error_cnt = 0){
  message(paste("error_cnt b4 tryCatch block entry:", error_cnt, sep = " "))
  tibble_obj<-tryCatch(
    av_get(symbol=ticker, av_fun="TIME_SERIES_INTRADAY", interval=interval, outputsize=outputsize),
    error = function(e) { error_cnt<-error_cnt+1; message(paste("there was an error, the error count is...", error_cnt, sep=" "));
    Sys.sleep(5); if_else(error_cnt > 3, return(NA), get_intraday_data_alphavantager(ticker, interval = "1min",outputsize = "full",
                                                                                      error_cnt = error_cnt))})
  return(tibble_obj)
  # use message() function instead of the print() function across the board
  # use stop() to stop exection if the error was a fatal one
  # use warning() to if error is not fatal, do not want to sop execution.
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
eod_batch_av_helper<-function(ticker, path_to_ticker_dir){
  require(lubridate)
  message(paste("begin intra minutely tick pull", ticker, sep=" "))
  file_path <- paste(path_to_ticker_dir, ticker, ".csv", sep="")
  remote<-FALSE
  if (file.exists(paste(path_to_ticker_dir, ticker, ".csv", sep = ""))){
    message(paste("file does exist...", path_to_ticker_dir, sep = ""))
    local_intraday_tibble<-as.tibble(read.csv(file=file_path, header=TRUE, sep=',', nrows=-1,
                                              colClasses = c("character", "POSIXct", "numeric",
                                                             "numeric", "numeric", "numeric", "integer")))
    message("tibble creation was successful...")
    local_intraday_tibble<-local_intraday_tibble[,2:ncol(local_intraday_tibble)]
    local_intraday_tibble<-local_intraday_tibble %>% setNames(., c("BarTimeStamp", "Open", "High", "Low", "Close", "Volume"))
    last_row_of_local<-local_intraday_tibble[nrow(local_intraday_tibble),]
    last_timestamp_local<-last_row_of_local$BarTimeStamp
    dst_bool<-dst(as.character(last_timestamp_local))
    if (dst_bool){last_timestamp_local<-as.POSIXct(last_timestamp_local, tz = "EDT");
    local_intraday_tibble$BarTimeStamp<-force_tz(local_intraday_tibble$BarTimeStamp, tzone = "EDT")
    } else { last_timestamp_local<-as.POSIXct(last_timestamp_local, tz = "EST");
    local_intraday_tibble$BarTimeStamp<-force_tz(local_intraday_tibble$BarTimeStamp, tzone = "EST") }
    save_original_tz<-as.POSIXct(last_timestamp_local)
    attr(last_timestamp_local, "tzone")<-"UTC"
    remote_intraday_tibble<-FALSE
    if (difftime(last_timestamp_local, when_was_mkt_open_last(), tz="UTC", units = c("mins")) < 0) {
      message(paste("file exists, but not up to date, going remote...", ticker, sep = ""))
      remote_intraday_tibble <- get_intraday_data_alphavantager(ticker, interval = "1min", outputsize = "full")
      remote<-TRUE
      if (!is.na(remote_intraday_tibble)){
        remote_intraday_tibble<-remote_intraday_tibble %>% setNames(c("BarTimeStamp", "Open", "High", "Low", "Close", "Volume"))
        if (dst_bool) { remote_intraday_tibble$BarTimeStamp<-force_tz(remote_intraday_tibble$BarTimeStamp, tzone = "EDT")
        } else { remote_intraday_tibble$BarTimeStamp<-force_tz(remote_intraday_tibble$BarTimeStamp, tzone = "EST") }
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
    remote_intraday_tibble <- get_intraday_data_alphavantager(ticker, interval = "1min", outputsize = "full")
    do.call("<-", list(paste(ticker, "intra", sep='.'), remote_intraday_tibble))
    custom_headers<-c("", "BarTimeStamp", "Open", "High", "Low", "Close", "Volume")
    write_intraday_av(ticker, get(paste(ticker, "intra", sep='.')), path_to_ticker_dir = path_to_ticker_dir, column_names = custom_headers)
  }
  # lets do some clean up if possible
  rm(remote_intraday_tibble);rm(local_intraday_tibble);
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

#' does a batch download at EOD for the days intraday timeseries. Checks to see
#' if we have a file for the ticker already on disk. See helper method for more
#' detail. If list of tickes is NA, then it gets set to the stocks 1000 space,
#' IWB holdings. List of tickers should be a vector of tickers, and get be
#' passed in by a call to tq_index or a call to getting a list of tickes from a
#' csv file.
#'
#' Currently, I am using this to do EOD batch runs at night of intraday
#' timeseries for all ETF's in my list and all IWB holdings.
#'
#' HARDCODES params!!!
#'
#' @param list_of_tickers
#'
#' @return nothing
#' @export
#'
#' @examples
eod_batch_av_intraday <- function(path_to_ticker_dir, path_to_api_key_file, list_of_tickers=NA){
  set_alphavantage_api_key(path_to_api_key_file)
  if (is.na(list_of_tickers)){
    IWB_holdings_file<-"/Users/ghazymahjub/workspace/data/IWB_holdings.csv"
    ETFS_VOL_OVER_1M_file<-"/Users/ghazymahjub/workspace/data/ETFS_VOL_OVER_1M.csv"
    IWB_holdings_tibble<-get_holdings_from_file_in_tibble_fmt(IWB_holdings_file)
    etfs_tibble<-as.tibble(read.csv(ETFS_VOL_OVER_1M_file, sep = ',', header=TRUE, nrows=-1))
    filtered_holdings_tibble<-get_holdings_filtered(IWB_holdings_tibble, "Asset.Class", "==", "Equity")
    list_of_tickers<-filtered_holdings_tibble$Ticker
    list_of_etf_tickers<-etfs_tibble$SYMBOL
    list_of_tickers<-c(as.character(list_of_etf_tickers), as.character(list_of_tickers))
  }
  sapply(list_of_tickers, FUN = eod_batch_av_intraday_error_helper, path_to_ticker_dir = path_to_ticker_dir)
  #sapply(list_of_tickers, FUN = eod_batch_av_helper, path_to_ticker_dir = path_to_ticker_dir)
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
eod_batch_av_intraday_error_helper<-function(ticker, path_to_ticker_dir){
  return (tryCatch(eod_batch_av_helper(ticker, path_to_ticker_dir),
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

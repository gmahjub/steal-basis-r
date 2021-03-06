#' connect2TWS
#'
#' @param port_number 
#'
#' @return
#' @export
#'
#' @examples
connect2TWS<-function(port_number = 7496){
  tws = twsConnect(port = port_number)
  return(tws)
}

connect2IBG<-function(port_number = 4001){
  ibg = ibgConnect(port = port_number)
  return (ibg)
}

#' isTWSConnected
#'
#' @param twsConnection 
#'
#' @return
#' @export
#'
#' @examples
isTwsConnected<-function(twsConnection){
  return (isConnected(twsConnection))
}

#' twsConnectionCreateTime
#'
#' @param twsConnection 
#'
#' @return
#' @export
#'
#' @examples
twsConnectionCreateTime<-function(twsConnection){
  timeOfConnection<-twsConnectionTime(twsConnection)
  return (timeOfConnection)  
}

#' disconnectTWSConn
#'
#' @param twsConnection 
#'
#' @return
#' @export
#'
#' @examples
disconnectTWSConn<-function(twsConnection){
  twsDisconnect(twsConnection)
}

#' resetTWSConnection
#'
#' @param twsConnection 
#'
#' @return a tws connection object
#' @export
#'
#' @examples
resetTWSConnection<-function(twsConnection){
  if (isTwsConnected(twsConnection)){
    disconnectTWSConn(twsConnection)
  }
  tws_conn_obj<-connect2TWS()
  return (tws_conn_obj)
}

#' getAccountUpdates
#'
#' @param twsConnection 
#'
#' @return
#' @export
#'
#' @examples
getAccountUpdate<-function(twsConnection){
  reqAccountUpdates(twsConnection)
}

#' getLiveMarketData
#'
#' @param twsConnection 
#' @param ticker 
#'
#' @return
#' @export
#'
#' @examples
getLiveMarketData<-function(twsConnection, ticker){
  security<-twsSTK(ticker)
  reqMktData(tws, security)
  # the above call will yield streaming data for the security with ticker as passed in parameter
}

#' getHistoricalData
#'
#' barSize must be in the following format: "<N S>" where S stands for seconds,
#' "<N D>" where D stands for days, "<N W>" where W stands for weeks, "<N M>"
#' where M stands for months, or "<N Y>" where Y stands for years. duration must
#' be in the following format: "<N secs>" if the duration is specified in
#' seconds. Otherwise, "<N mins>" when N>1, if N = 1 then "<N min>". Feel free
#' to sub "mins" with "hours", "days", "weeks", "months".
#'
#' @param twsConnection tws connection object, a result of a call to connect2TWS
#' @param ticker ticker as string
#' @param barSize see above for valid barSize arguement
#' @param whatToShow 
#' @param write_out TRUE/FALSE
#' @param path_to_ticker_dir 
#' @param duration see above for valid duration argument
#'
#' @return an xts object contianing the timeseries historical data.
#' @export
#'
#' @examples
getHistoricalData<-function(ticker, barSize = "1 min", duration = '1 W', whatToShow = "TRADES", contractType = "E", write_out = FALSE, path_to_ticker_dir = NA, 
                            error_log_file = NA, port_number = 4001, end_date_time = NA){
  error_flag = FALSE
  message(paste("pulling ticker ", ticker, sep = ""))
  ibgConnection<-connect2IBG(port_number = port_number)
  if (contractType == "E")
    con_obj<-twsEquity(ticker)
  else if (contractType == "F")
    con_obj<-twsFuture()
  con_details<-tryCatch(reqContractDetails(ibgConnection, twsEquity(ticker)), warning = function(w) { message(paste(ticker, " is possibly an invalid ticker...", sep = ""))})
  if (length(con_details) == 0){
    message(paste(ticker, " request for contract details returned an empty list", sep = ""))
    disconnectTWSConn(ibgConnection)
    return (NULL)
  } else {
    primary_exch<-tryCatch(con_details[[1]]$contract$primary, error = function(e) { message(paste(ticker, ": not able to retrieve primary exchange.", sep = ""));
      write_error_log(ticker, error_log_file, message = paste("primary exchange not able to retrieve, message was: ", e, sep = "")); "FAILED"})
  }
  if (primary_exch == "FAILED"){
    disconnectTWSConn(ibgConnection)
    write_error_log(ticker, error_log_file, message = "primary exchange lookup failed!")
  } else {
    message(paste("primary exchange for ", ticker, " is ", primary_exch, sep = ""))
    contract_obj<-twsSTK(ticker, primary = primary_exch)
    if (is.na(end_date_time)) {
      ticker_hist_data<-tryCatch(reqHistoricalData(ibgConnection, contract_obj, whatToShow = whatToShow, barSize = barSize, duration = duration), 
                                 error = function(e) {message(paste(ticker, "Failed", sep = ",")); write_error_log(ticker, error_log_file)})
    }
    else 
      ticker_hist_data<-tryCatch(reqHistoricalData(ibgConnection, contract_obj, whatToShow = whatToShow, barSize = barSize, duration = duration, endDateTime = end_date_time),
                                 error = function(e) {message(paste(ticker, "Failed", sep = ",")); write_error_log(ticker, error_log_file)})
    if (write_out) {
      disconnectTWSConn(ibgConnection)
      ticker_csv_file<-paste(path_to_ticker_dir, ticker, ".csv", sep = "")
      tryCatch(write.zoo(as.xts(ticker_hist_data), ticker_csv_file, index.name = "Date", sep=","), error = function(e) { message(paste(ticker, "write to csv failed", sep = " "))})
    } else {
      disconnectTWSConn(ibgConnection)
      return(ticker_hist_data)
    }
  }
}

#' getIbkrDaily_xts
#'
#' @param ticker 
#' @param path_to_ticker_dir 
#'
#' @return
#' @export
#'
#' @examples
getIbkrDaily_xts<-function(ticker, path_to_ticker_dir, return_format = "xts"){
  file_path <- paste(path_to_ticker_dir, ticker, ".csv", sep = "")
  if (file.exists(file_path)){
    message(paste("file does exist...", path_to_ticker_dir, sep = ""))
    local_intraday_tibble<-as.tibble(read.csv(file = file_path, header = TRUE, sep = ',', nrows = -1,
                                              colClasses = c("Date", "numeric", "numeric", "numeric", "numeric", "integer", 
                                                              "numeric", "integer", "integer")))
    local_intraday_tibble<-local_intraday_tibble[,1:ncol(local_intraday_tibble)]
    last_row_of_local<-local_intraday_tibble[nrow(local_intraday_tibble),]
    last_timestamp_local<-last_row_of_local$BarTimeStamp
    when_was_mkt_open_last_value<-date(when_was_mkt_open_last())
    save_original_tz <- last_timestamp_local
    remote_intraday_tibble<-FALSE
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
      # file is up to date, return the file subsetted accordingly, in tibble or xts, accordingly.
      ibkr_daily_xts<-as.xts(read.zoo(file=file_path, header = TRUE, sep = ",", nrows = -1,
                                       colClasses = c("Date", "numeric", "numeric", "numeric", "numeric", "integer",
                                                      "numeric", "integer", "integer")))
      if (return_format == "tibble"){
        the_tibble<-convertYahoo_xts2tibble(ticker, ibkr_daily_xts)
        return (the_tibble)
      }
      return (ibkr_daily_xts)
    }
  } else {
    # file does not exist, therefore, download all the data.
    save_original_tz <- NULL
    ibkr_daily_xts<-getHistoricalData(ticker, barSize = "1 day", duration = '20 Y', whatToShow = IBKR_px_type, 
                                              write_out = FALSE, path_to_ticker_dir = path_to_ticker_dir, 
                                              error_log_file = error_log, port_number = 4001, end_date_time = "")
    do.call("<-", list(paste(ticker, "intra", sep='.'), ibkr_daily_xts))
    write_intraday_IBKR(ticker, get(paste(ticker, "intra", sep = ".")), path_to_ticker_dir = path_to_ticker_dir, intraday = FALSE)
    if (return_format == "tibble"){
      the_tibble<-convertYahoo_xts2tibble(ticker, ibkr_daily_xts)
      return (the_tibble)
    }
    return (ibkr_daily_xts)
  }
  # below is executed if there was a file locally already, and the file needed to be updated.
  if (!is.null(remote_intraday_tibble)){
    remote_intraday_tibble <- tk_tbl(remote_intraday_tibble, rename_index = "BarTimeStamp")
    remote<-TRUE
    if (!is.null(remote_intraday_tibble) && !is.null(save_original_tz)){
      new_timeseries_to_append<-remote_intraday_tibble %>% filter(BarTimeStamp > save_original_tz)
      intraday_tibble_obj<-rbind(local_intraday_tibble, new_timeseries_to_append)
      intraday_tibble_obj<-tk_xts(intraday_tibble_obj, date_var = BarTimeStamp, select = -BarTimeStamp)
      do.call("<-", list(paste(ticker, "intra", sep='.'), intraday_tibble_obj))
      write_intraday_IBKR(ticker, get(paste(ticker, "intra", sep='.')), path_to_ticker_dir = path_to_ticker_dir, intraday = FALSE)
      return (intraday_tibble_obj)
    }
  }
}

#' getFutContractObject
#'
#' Return the contract object based on provided symbol, using reqContractDetails
#' of IBrokers.
#'
#' @param symbol
#'
#' @return twsContract object
#' @export
#'
#' @examples
getFutContractObject<-function(symbol){
  twsConnection<-connect2TWS()
  con_obj<-twsFuture(symbol = symbol, exch = "", expiry = "")
  con_details<-reqContractDetails(twsConnection, con_obj)
  twsContract_obj<-con_details[[1]]$contract
  disconnectTWSConn(twsConnection)
  return (twsContract_obj)
}

#' getFxContractObject
#'
#' @param symbol 
#' @param currency 
#' @param secType 
#' @param exchange 
#'
#' @return
#' @export
#'
#' @examples
getFxContractObject<-function(symbol = "EUR", currency = "USD", secType = "CASH", exchange = "IDEALPRO"){
  twsConnection<-connect2TWS()
  # eg. EUR.USD is symbol = "EUR", currency = "USD"
  con_obj<-twsCurrency(symbol = symbol, currency = base_curr)
  con_details<-reqContractDetails(twsConnection, con_obj)
  twsContract_obj<-con_details[[1]]$contract
  disconnectTWSConn(twsConnection)
  return (twsContract_obj)
}

#' getHistoricalData_futs
#' 
#' Get historical data for a symbol, returned in xts format or written to csv file.
#'
#' @param symbol 
#' @param barSize 
#' @param duration 
#' @param whatToShow 
#' @param write_out 
#' @param path_to_ticker_dir 
#' @param error_log_file 
#' @param port_number 
#'
#' @return
#' @export
#'
#' @examples
getHistoricalData_futs<-function(symbol, barSize = "1 min", duration = '1 W', whatToShow = "TRADES", secType = "CONTFUT", write_out = FALSE, 
                                    path_to_ticker_dir = NA, error_log_file = NA, port_number = 7496, useRTH = 0, end_date_time = NA) {
  # continuous futures only supported on TWS Version 971 or higher, not available on IB Gateway
  error_flag = FALSE
  message(paste("pulling continuous futures ticker ", symbol, sep = ""))
  twsConnection<-connect2TWS(port_number = port_number)
  con_obj<-twsFuture(symbol = symbol, exch = "", expiry = "")
  con_details<-reqContractDetails(twsConnection, con_obj)
  twsContract_obj<-con_details[[1]]$contract
  twsContract_obj$sectype<-secType
  if (is.na(end_date_time))
    symbol_hist_data<-tryCatch(reqHistoricalData(twsConnection, twsContract_obj, whatToShow = whatToShow, barSize = barSize, duration = duration, useRTH = useRTH), 
                               error = function(e) {message(paste(symbol, "Failed", sep = ",")); write_error_log(ticker, error_log_file)})
  else 
    symbol_hist_data<-tryCatch(reqHistoricalData(twsConnection, twsContract_obj, whatToShow = whatToShow, barSize = barSize, endDateTime = end_date_time,
                                                 duration = duration, useRTH = useRTH), error = function(e) {message(paste(symbol, "Failed", sep = ",")); 
                                                   write_error_log(ticker, error_log_file)})
  if (write_out) {
    disconnectTWSConn(twsConnection)
    symbol_csv_file<-paste(path_to_ticker_dir, symbol, ".csv", sep = "")
    tryCatch(write.zoo(as.xts(symbol_hist_data), symbol_csv_file, index.name = "Date", sep=","), error = function(e) { 
      message(paste(symbol, "write to csv failed", sep = " "))})
  } else {
    disconnectTWSConn(twsConnection)
    return(symbol_hist_data)
  }
}

#' getHistoricalData_forex
#'
#' @param symbol transaction (numerator) currency
#' @param currency base (denomincator) currency
#' @param barSize 
#' @param duration 
#' @param whatToShow 
#' @param secType 
#' @param write_out 
#' @param path_to_ticker_dir 
#' @param error_log_file 
#' @param port_number 
#'
#' @return
#' @export
#'
#' @examples
getHistoricalData_forex<-function(symbol, currency, barSize = "1 min", duration = '1 W', whatToShow = "MIDPOINT", secType = "CASH", write_out = FALSE,
                                  path_to_ticker_dir = NA, error_log_file = NA, port_number = 7496, end_date_time = NA){
  error_flag = FALSE
  message(paste("pulling FOREX ticker ", symbol, sep = ""))
  twsConnection<-connect2TWS(port_number = port_number)
  con_obj<-twsCurrency(symbol = symbol, currency = currency)
  con_details<-reqContractDetails(twsConnection, con_obj)
  twsContract_obj<-con_details[[1]]$contract
  twsContract_obj$sectype<-secType
  if (is.na(end_date_time))
    symbol_hist_data <- tryCatch(reqHistoricalData(twsConnection, twsContract_obj, whatToShow = whatToShow, barSize = barSize, duration = duration), 
                                 error = function(e) {message(paste(symbol, "FAILED", sep = ",")); write_error_log(ticker, error_log_file)})
  else
    symbol_hist_date <- tryCatch(reqHistoricalData(twsConnection, twsContract_obj, whatToShow = whatToShow, barSize = barSize, duration = duration, endDateTime = end_date_time), 
                                 error = function(e) {message(paste(symbol, "FAILED", sep = ",")); write_error_log(ticker, error_log_file)})
  if (write_out) {
    disconnectTWSConn(twsConnection)
    symbol_csv_file<-paste(path_to_ticker_Dir, symbol, ".csv", sep = "")
    tryCatch(write.zoo(as.xts(symbol_hist_data), symbol_csv_file, index.name = "Date", sep = ","), error = function(e) {
      message(paste(symbol, "write to csv failed", sep = " "))})
  } else {
    disconnectTWSConn(twsConnection)
    return (symbol_hist_data)
  }
}

#' calcBarOhlcMinusBarWap
#'
#' WAP data is only included in XTS from getHistoricalData when the whatToShow
#' field is set to "TRADES". WAP is a volume weighted avaerage price (VWAP) of the bar.
#' In this function, we are looking to see where the VWAP is relative to the high and low
#' px of the bar. 
#'
#' @param ticker
#' @param ticker_hist_data
#'
#' @return
#' @export
#'
#' @examples
calcBarOhlcMinusBarWap<-function(ticker, historicalData){
  col_headers<-names(historicalData)
  op_px_series<-Op(historicalData)
  hi_px_series<-Hi(historicalData)
  lo_px_series<-Lo(historicalData)
  mid_px_series<-(hi_px_series + lo_px_series)/2.0
  cl_px_series<-Cl(historicalData)
  cl_rets_series<-ROC(cl_px_series)*100.0
  cl_rets_lagged_series<-lag.xts(cl_rets_series, k=-1)
  # WAP is a weighted average price of all the trade prices (or whatever type selected), weighted by volume
  # there is also a bar.count column, that column is the number of trades in the bar.
  # the Volume column can NEVER be less than the count column.
  wap_header<-paste(ticker, "WAP", sep = ".")
  wap_px_series<-historicalData[, wap_header]
  # cost basis price = Open
  wap_op_ret_series<-(wap_px_series-op_px_series)/op_px_series*100.0
  mid_op_ret_series<-(mid_px_series-op_px_series)/op_px_series*100.0
  hi_op_ret_series<-(hi_px_series-op_px_series)/op_px_series*100.0
  lo_op_ret_series<-(lo_px_series-op_px_series)/op_px_series*100.0
  cl_op_ret_series<-(cl_px_series-op_px_series)/op_px_series*100.0
  # if most of the trading was done near the high price, and the close price of this bar is lower than the WAP price,
  # then we can say that the stock was being sold at the WAP price.
  hi_wap_ret_series<-(hi_px_series-wap_px_series)/wap_px_series*100.0
  # if most of the trading was done near the low price, and the close price of this bar is higher than the WAP price,
  # then we can say that people were buying the stock
  lo_wap_ret_series<-(lo_px_series-wap_px_series)/wap_px_series*100.0
  # terminal price = close
  cl_wap_ret_series<-(cl_px_series-wap_px_series)/wap_px_series*100.0
  cl_mid_ret_series<-(cl_px_series-mid_px_series)/mid_px_series*100.0
  cl_hi_ret_series<-(cl_px_series-hi_px_series)/hi_px_series*100.0
  cl_lo_ret_series<-(cl_px_series-lo_px_series)/lo_px_series*100.0
  # Indicators
  wap_hilo_relative_series<-(wap_px_series - lo_px_series)/(hi_px_series - lo_px_series)*100.0
  cl_hilo_relative_series<-(cl_px_series - lo_px_series)/(hi_px_series - lo_px_series)*100.0
  cl2wap_hilo_relaive_series<-(cl_px_series-wap_px_series)/(hi_px_series - lo_px_series)*100.0
  # build the return xts
  return_xts<-merge(wap_op_ret_series, mid_op_ret_series)
  return_xts<-merge(return_xts, hi_op_ret_series)
  return_xts<-merge(return_xts, lo_op_ret_series)
  return_xts<-merge(return_xts, cl_op_ret_series)
  return_xts<-merge(return_xts, hi_wap_ret_series)
  return_xts<-merge(return_xts, lo_wap_ret_series)
  return_xts<-merge(return_xts, cl_wap_ret_series)
  return_xts<-merge(return_xts, cl_mid_ret_series)
  return_xts<-merge(return_xts, cl_hi_ret_series)
  return_xts<-merge(return_xts, cl_lo_ret_series)
  return_xts<-merge(return_xts, cl_px_series)
  return_xts<-merge(return_xts, cl_rets_series)
  return_xts<-merge(return_xts, cl_rets_lagged_series)
  return_xts<-merge(return_xts, wap_hilo_relative_series)
  return_xts<-merge(return_xts, cl_hilo_relative_series)
  return_xts<-merge(return_xts, cl2wap_hilo_relaive_series)
  colnames(return_xts)<-c(paste(ticker, "op2wap_ret", sep = "."), paste(ticker, "op2mid_ret", sep = "."), paste(ticker, "op2hi_ret", sep = "."), 
                          paste(ticker, "op2lo_ret", sep = "."), paste(ticker, "op2cl_ret", sep = "."), paste(ticker, "wap2hi_ret", sep = "."), 
                          paste(ticker, "wap2lo_ret", sep = "."), paste(ticker, "wap2cl_ret", sep = "."), paste(ticker, "mid2cl_ret", sep = "."), 
                          paste(ticker, "hi2cl_ret", sep = "."), paste(ticker, "lo2cl_ret", sep = "."), names(cl_px_series), 
                          paste(ticker, "ClCl.Rets", sep = "."), paste(ticker, "ClCl.Rets.1Lag", sep = "."), paste(ticker, "WAPRelative2HiLo", sep = "."),
                          paste(ticker, "ClRelative2HiLo", sep = "."), paste(ticker, "Cl2WapRelative2HiLo", sep = "."))
  return(return_xts)
}

#' hilo_price_autocorr
#'
#' @param ticker 
#' @param historicalData 
#'
#' @return
#' @export
#'
#' @examples
hilo_price_autocorr<-function(ticker, historicalData){
  hi_price_lagged<-lag(Hi(historicalData), k =-1)
  lo_price_lagged<-lag(Lo(historicalData), k = -1)
  test_1<-and(Hi(historicalData) >= lo_price_lagged, Hi(historicalData) < hi_price_lagged) == 1
  test_2<-Hi(historicalData)<lo_price_lagged
  test_neg1<-and(Lo(historicalData) <= hi_price_lagged, Lo(historicalData) > lo_price_lagged) == 1
  test_neg2<-Lo(historicalData) > hi_price_lagged
  hilo_price_autocorr_ind<-ifelse(test_1, 1, ifelse(test_2, 2, ifelse(test_neg1, -1, ifelse(test_neg2, -2, 0))))
  colnames(hilo_price_autocorr_ind)<-c(paste(ticker, "HiLoACind", sep = "."))
  return (hilo_price_autocorr_ind)
}

#' filter_WAPrelative2HiLo
#' 
#' Part of WAP workflow. Input into this function is result of a call to calcBarOhlcMinusBarWap() function.
#' This function will filter out WAPIndicator values, based on the input parameters "high_th" and "low_th".
#' Think of these input parameters as similar to RSI thresholds (60/40). Output from this function
#' is simply the orginal input barOhlcMinusBarWap with 1 addtl column appended, with columns
#' header "<ticker>.WAP.Trade.Sig". 
#'
#' @param ticker ticker as string.
#' @param barOhlcMinusBarWap_xts xts, result of call to calcBarOhlcMinusBarWap()
#' @param high_th float, default is 60.0
#' @param low_th float, default is 40.0
#'
#' @return xts, same as original xts, but with 1 addtl column appended.
#' @export
#'
#' @examples
filter_WAPrelative2HiLo<-function(ticker, barOhlcMinusBarWap_xts, high_th = 60.0, low_th = 40.0){
  wap_ind_col_name<-paste(ticker, "WAPRelative2HiLo", sep = ".")
  clcl_ret_1lag_colname<-paste(ticker, "ClCl.Rets.1Lag", sep = ".")
  wap_ind_series<-barOhlcMinusBarWap_xts[, wap_ind_col_name]
  clcl_ret_1lag_series<-barOhlcMinusBarWap_xts[, clcl_ret_1lag_colname]
  trade_sig<-ifelse(wap_ind_series>high_th,-1,ifelse(wap_ind_series<low_th, 1, 0))
  trade_ret<-ifelse(trade_sig == 1, barOhlcMinusBarWap_xts[, clcl_ret_1lag_colname], 
                    ifelse(trade_sig == -1, barOhlcMinusBarWap_xts[,clcl_ret_1lag_colname]*-1.0, 0.0))
  colnames(trade_sig)<-paste(ticker, "WAP.Trade.Sig", sep = ".")
  colnames(trade_ret)<-paste(ticker, "WAP.Trade.Ret", sep = ".")
  return_xts<-merge(barOhlcMinusBarWap_xts, trade_sig)
  return_xts<-merge(return_xts, trade_ret)
  return(return_xts)
}

filter_cl2wap_ret<-function(ticker, barOhlcMinusBarWap_xts, cl2wap_hilo_relative_th){
  wap_ind_col_name<-paste(ticker, "Cl2WapRelative2HiLo", sep = ".")
  clcl_ret_1lag_colname<-paste(ticker, "ClCl.Rets.1Lag", sep = ".")
  wap_ind_series<-barOhlcMinusBarWap_xts[, wap_ind_col_name]
  clcl_ret_1lag_series<-barOhlcMinusBarWap_xts[, clcl_ret_1lag_colname]
  filtered_wap2cl_hilo_relative<-ifelse(abs(wap_ind_col_name)>wap2cl_hilo_relative_th, 1, 0)
  colnames(filtered_wap2cl_hilo_relative)<-paste(ticker, "Cl2WapRelative2HiLoExceeds", cl2wap_hilo_relative_th, sep = ".")
  return_xts<-merge(barOhlcMinusBarWap_xts, filtered_wap2cl_hilo_relative)
  return(return_xts)
}

plot_xts_reset<-function(num_rows){
  par(mfrow=c(num_rows,1))
}

#' plot_WAPIndicator
#'
#' Not a great function, just run its contents in a console to get 2 plots,
#' row-wise, the first being the closing price, and the second containg both the
#' WAP.Trade.Sig and the 1lag Return of closing price.
#'
#' @param ticker
#' @param filteredWAPIndicator_xts result of call to filterWAPIndicatorValues()
#'
#' @return
#' @export
#'
#' @examples
plot_WAPIndicator<-function(ticker, filteredWAPIndicator_xts){
  to_plot<-merge(filteredWAPIndicator_xts[,5], filteredWAPIndicator_xts[,7], filteredWAPIndicator_xts[,9])
  par(mfrow=c(1,1))
  plot.xts(to_plot[,1], observation.based = TRUE)
  plot.xts(to_plot[,2:3], observation.based = TRUE)
}

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
getHistoricalData<-function(ticker, barSize = "1 min", duration = '1 W', whatToShow = "TRADES", write_out = FALSE, path_to_ticker_dir = NA, 
                            error_log_file = NA){
  message(paste("pulling ticker ", ticker, sep = ""))
  twsConnection<-connect2TWS()
  con_details<-reqContractDetails(twsConnection, twsEquity(ticker))
  primary_exch<-tryCatch(con_details[[1]]$contract$primary, error = function(e) { message(paste(ticker, ": not able to retrieve primary exchange.", sep = ""));
    write_error_log(paste(ticker, " primary exchange not able to retrieve, message was: ", e, sep = ""))})
  message(paste("primary exchange for ", ticker, " is ", primary_exch, sep = ""))
  contract_obj<-twsSTK(ticker, primary = primary_exch)
  ticker_hist_data<-tryCatch(reqHistoricalData(twsConnection, contract_obj, whatToShow = whatToShow, barSize = barSize, 
                                      duration = duration), error = function(e) {message(paste(ticker, "Failed", sep = ",")); write_error_log(ticker, error_log_file); Sys.sleep(12)})
  #disconnectTWSConn(twsConnection)
  if (write_out) {
    disconnectTWSConn(twsConnection)
    ticker_csv_file<-paste(path_to_ticker_dir, ticker, ".csv", sep = "")
    tryCatch(write.zoo(as.xts(ticker_hist_data), ticker_csv_file, index.name = "Date", sep=","), error = function(e) { message(paste(ticker, "write to csv failed", sep = " "))})
  } else {
    disconnectTWSConn(twsConnection)
    return(ticker_hist_data)
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

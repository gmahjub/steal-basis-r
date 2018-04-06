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
#' @param duration see above for valid duration argument
#' @param price_type valid options: TRADES, MIDPOINT, BID, ASK, BID_ASK,
#'   ADJUSTED_LAST, HISTORICAL_VOLATILITY, OPTION_IMPLIED_VOLATILITY,
#'   REBATE_RATE, FEE_RATE, YIELD_BID, YIELD_ASK, YIELD_BID_ASK, YIELD_LAST
#'
#' @return an xts object contianing the timeseries historical data.
#' @export
#'
#' @examples
getHistoricalData<-function(twsConnection, ticker, barSize = "1 min", duration = '1 W', whatToShow = "TRADES"){
  contract_obj<-twsSTK(ticker)
  ticker_hist_data<-reqHistoricalData(twsConnection, contract_obj, whatToShow = whatToShow, barSize = barSize, 
                                      duration = duration)
  # when whatToShow is "TRADES", we have extra information to use. The volume traded in each bar, the WAP, which is the volume
  # weighted price of each bar, and the number of trades that occurred in that bar (ticker.<count>)
  return(ticker_hist_data)
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
  cl_px_series<-Cl(historicalData)
  cl_rets_series<-ROC(cl_px_series)*100.0
  cl_rets_lagged_series<-lag(cl_rets_series, k=-1)
  # WAP is a weighted average price of all the trade prices (or whatever type selected), weighted by volume
  # there is also a bar.count column, that column is the number of trades in the bar.
  # the Volume column can NEVER be less than the count column.
  wap_header<-paste(ticker, "WAP", sep = ".")
  wap_px_series<-historicalData[, wap_header]
  op_wap_diff_series<-(op_px_series-wap_px_series)/wap_px_series*100.0
  # if most of the trading was done near the high price, and the close price of this bar is lower than the WAP price,
  # then we can say that the stock was being sold at the WAP price.
  hi_wap_diff_series<-(hi_px_series-wap_px_series)/wap_px_series*100.0
  # if most of the trading was done near the low price, and the close price of this bar is higher than the WAP price,
  # then we can say that people were buying the stock 
  lo_wap_diff_series<-(lo_px_series-wap_px_series)/wap_px_series*100.0
  cl_wap_diff_series<-(cl_px_series-wap_px_series)/wap_px_series*100.0
  wap_ind_series<-(wap_px_series - lo_px_series)/(hi_px_series - lo_px_series)*100.0
  return_xts<-merge(op_wap_diff_series, hi_wap_diff_series)
  return_xts<-merge(return_xts, lo_wap_diff_series)
  return_xts<-merge(return_xts, cl_wap_diff_series)
  return_xts<-merge(return_xts, cl_px_series)
  return_xts<-merge(return_xts, cl_rets_series)
  return_xts<-merge(return_xts, cl_rets_lagged_series)
  return_xts<-merge(return_xts, wap_ind_series)
  colnames(return_xts)<-c(paste(ticker, "op_wap_ret", sep = "."), paste(ticker, "hi_wap_ret", sep = "."), paste(ticker, "lo_wap_ret", sep = "."), 
                          paste(ticker, "cl_wap_ret", sep = "."), names(cl_px_series), paste(ticker, "ClCl.Rets", sep = "."), 
                          paste(ticker, "ClCl.Rets.1Lag", sep = "."), paste(ticker, "WAPIndicator", sep = "."))
  return(return_xts)
}

#' filterWAPIndicatorValues
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
filterWAPIndicatorValues<-function(ticker, barOhlcMinusBarWap_xts, high_th = 60.0, low_th = 40.0){
  wap_ind_col_name<-paste(ticker, "WAPIndicator", sep = ".")
  wap_ind_series<-barOhlcMinusBarWap_xts[, wap_ind_col_name]
  trade_sig<-ifelse(wap_ind_series>high_th,-1,ifelse(wap_ind_series<low_th, 1, 0))
  colnames(trade_sig)<-paste(ticker, "WAP.Trade.Sig", sep = ".")
  return_xts<-merge(barOhlcMinusBarWap_xts, trade_sig)
  return(return_xts)
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
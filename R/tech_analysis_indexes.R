calc_NVI<-function(ticker){
  nvi <<- 100.0
  xts_obj<-get(ticker)
  volume_diff<-diff(Vo(xts_obj))
  adj_close<-Ad(xts_obj)
  adjCl_Rets<-Return.calculate(adj_close)
  y<-merge(volume_diff, adjCl_Rets)
  y<-na.omit(y)
  colnames(y)<-c("volume_diff", "AdjCl_Rets")
  volume_diff_list<-y$volume_diff
  adjCl_Rets_list<-y$AdjCl_Rets
  return_nvi<-mapply(NVI_helper, volume_diff_list, adjCl_Rets_list)
  y_final<-merge(y, return_nvi)
  colnames(y_final)<-c(colnames(y), paste(ticker, "NVI", sep = "."))
  return(y_final[,3])
}

calc_PVI<-function(ticker){
  pvi<<-100.0
  xts_obj<-get(ticker)
  volume_diff<-diff(Vo(xts_obj))
  adj_close<-Ad(xts_obj)
  adjCl_Rets<-Return.calculate(adj_close)
  y<-merge(volume_diff, adjCl_Rets)
  y<-na.omit(y)
  colnames(y)<-c("volume_diff", "AdjCl_Rets")
  volume_diff_list<-y$volume_diff
  adjCl_Rets_list<-y$AdjCl_Rets
  return_pvi<-mapply(PVI_helper, volume_diff_list, adjCl_Rets_list)
  y<-merge(y, return_pvi)
  return(y)
}

NVI_helper<-function(volume_diff, adjCl_Rets){
  ifelse(volume_diff<=0, nvi<<-nvi, nvi<<-(adjCl_Rets + 1)*nvi)
}

PVI_helper<-function(volume_diff, adjCl_Rets){
  ifelse(volume_diff>0, pvi<<-pvi, pvi<<-(adjCl_Rets + 1)*pvi)
}

#' calc_RSI
#'
#' Returns a time series representing the RSI of input time series. The key
#' parameter for careful consideration and utilization is the maType. Here we
#' can specify the type of smoothing that we use, either the same for both price
#' returns (up/positive/down/negative) or individually specified. The maType
#' parameter is of type list, and each individual parameter for maUp and maDown,
#' is also a list. The parameter format was modeled after the TTR::RSI function,
#' so vew help(TTR::RSI)  when formulating use of this wrapper function.
#'
#' @param ticker type string, the ticker of the stock.
#' @param px_type options are 'O/Open', 'H/High', 'L/Low', 'C/Close', 'A/AdjCl',
#'   'V/Volume'
#' @param ma_period_size window size in mumber of periods, default is 14
#' @param maType type list, for each (maUp/maDown), many options including EMA,
#'   SMA, VWMA, etc.. see Help(EMA) for full list.
#'
#' @return time series, type xts, with RSI values
#' @export
#'
#' @examples
calc_RSI<-function(ticker, px_type = "A", ma_period_size = 14,  
                   maType = list(maUp=list(EMA, ratio=1/5), 
                                 maDown = list(EMA, ratio=1/5))){
  xts_obj<-get(ticker)
  px_series<-Ad(xts_obj)
  if (px_type == "O"){ price_series<-Op(xts_obj) 
  } else if (px_type == "H") { px_series<-Hi(xts_obj) 
  } else if (px_type == "L") { px_series<-Lo(xts_obj) 
  } else if (px_type == "C") { px_series<-Cl(xts_obj) 
  } else if (px_type == "V") { px_series<-Vo(xts_obj) } 
  #rsi_series<-RSI(px_series)
  rsi_series<-RSI(px_series, n = ma_period_size, maType = maType)
  return (rsi_series)
}
calc_period_Median_price<-function(ticker, px_type = "A", period_type = 'weeks', num_periods = 1){
  # TTR::runMedian()
  xts_obj<-get(ticker)
  px_series<-Ad(xts_obj)
  if (px_type == "O"){ px_series <- Op(xts_obj) }
  else if (px_type == "H") { px_series <- Hi(xts_obj) }
  else if (px_type == "L") { px_series <- Lo(xts_obj) }
  else if (px_type == "C") { px_series <- Cl(xts_obj) }
  ep <- endpoints(px_series, period_type, k = num_periods)
  period_median<-period.apply(px_series, INDEX = ep, FUN = median)
  colnames(period_median)<-paste(ticker, num_periods, period_type, "Median", sep = ".")
  return(period_median)
}

calc_rolling_Median_price<-function(ticker, px_type = "A", window_size = 5){
  xts_obj<-get(ticker)
  px_series<-Ad(xts_obj)
  if (px_type == "O"){ px_series <- Op(xts_obj) }
  else if (px_type == "H") { px_series <- Hi(xts_obj) }
  else if (px_type == "L") { px_series <- Lo(xts_obj) }
  else if (px_type == "C") { px_series <- Cl(xts_obj) }
  median_px_series<-runMedian(px_series, n = window_size)
  return (median_px_series)
}

calc_period_Highest_High_price<-function(ticker, period_type = 'weeks', num_periods=1){
  xts_obj<-get(ticker)
  px_series<-Hi(xts_obj)
  ep <- endpoints(px_series, period_type, k = num_periods)
  period_highest_high <- period.apply(px_series, INDEX = ep, FUN = max )
  colnames(period_highest_high) <- paste(ticker, num_periods, period_type, "Highest_High", sep = ".")
  return(period_highest_high)
}

calc_rolling_Highest_High_price<-function(ticker, window_size = 5){
  xts_obj<-get(ticker)
  px_series<-Hi(xts_obj)
  highest_high_px_series<-runMax(px_series, n = window_size)
  colnames(highest_high_px_series) <- paste(ticker, window_size, "Rolling_High", sep = ".")
  return(highest_high_px_series)
}

calc_period_Lowest_Low_price<-function(ticker, period_type = 'weeks', num_periods = 1){
  xts_obj <- get(ticker)
  px_series<-Lo(xts_obj)
  ep <- endpoints(px_series, period_type, k = num_periods)
  period_lowest_low <- period.apply(px_series, INDEX = ep, FUN = min)
  colnames(period_lowest_low) <- paste(ticker, num_periods, period_type, "Lowest_Low", sep = ".")
  return (period_lowest_low)
}

cal_rolling_Lowest_Low_price<-function(ticker, window_size = 5){
  xts_obj <- get(ticker)
  px_series <- Lo(xts_obj)
  lowest_low_px_series<-runMIn(px_series, n = window_size)
  colnames(lowest_low_px_series)<-paste(ticker, window_size, "Rolling_Low", sep = ".")
  return(lowest_low_px_series)
}

#' calc_modified_moving_average
#'
#' Calculate the Modified Moving Average of the input univariate timeseries
#' object. Window size is the number of samples to use in the moving average
#' calculation. This funciton uses pracma::movavg. Requires pracma. Use this
#' funciton after calcuating runMedian. Take the MMA of runMedian result from
#' above.
#'
#' @param univariate_xts
#' @param window_size
#'
#' @return a univariate xts, labed <ticker>.mma
#' @export
#'
#' @examples
calc_modified_moving_average<-function(univariate_xts, window_size = 5 ){
  mma<-pracma::movavg(as.vector(univariate_xts), n = window_size, type = 'm')
  mma<-merge(univariate_xts, mma)
  colnames(mma)<-c(colnames(univariate_xts), paste(ticker, "mma", sep = '.'))
  return(mma[,2])
}

#' calc_PVT
#' 
#' Price Volume Trend Indicator
#'
#' @param ticker 
#'
#' @return
#' @export
#'
#' @examples
calc_PVT<-function(ticker){
  xts_obj<-get(ticker)
  pvt<<-0
  volume<-Vo(xts_obj)
  adj_close<-Ad(xts_obj)
  adjCl_Rets<-Return.calculate(adj_close)
  y<-merge(volume, adjCl_Rets)
  colnames(y)<-c("Volume", "AdjCl_Rets")
  y<-na.omit(y)
  volume_vec<-y$Volume
  rets_vec<-y$AdjCl_Rets
  return_pvt<-mapply(PVT_helper, volume_vec, rets_vec)
  y_final<-merge(y, return_pvt)
  colnames(y_final)<-c(colnames(y), paste(ticker, "PVT", sep = "."))
  return(y_final[,3])
}

#' PVT_helper
#' 
#' 
#' @param volume 
#' @param rets 
#'
#' @return
#' @export
#'
#' @examples
PVT_helper<-function(volume, rets){
  pvt<<-(pvt + volume*rets)
}

#' calc_BBands
#'
#' @param ticker 
#' @param px_type 
#' @param window_size 
#' @param sd 
#' @param maType 
#' @param ratio 
#'
#' @return
#' @export
#'
#' @examples
calc_BBands<-function(ticker, px_type = "HLC", window_size = 20, sd = 2, maType = EMA, ratio = 1/5){
  xts_obj<-get(ticker)
  adj_cl_px_series<-Ad(xts_obj)
  px_series <- HLC(xts_obj)
  if (px_type == "A"){
    px_series <-Ad(xts_obj)
  } else if (px_type == "O"){
    px_series <- Op(xts_obj)
  } else if (px_type == "H"){
    px_series <- Hi(xts_obj)
  } else if (px_type == "L"){
    px_series <- Lo(xts_obj)
  } else if (px_type == "C"){
    px_series <- Cl(xts_obj)
  }
  BBands_xts <- BBands(px_series, maType = maType, sd = sd)
  returned_xts<-merge(adj_cl_px_series, BBands_xts)
  return (returned_xts)
}

calc_PercentB<-function(ticker, px_type = "HLC", window_size = 20, sd = 2, maType = EMA, ratio = 1/5){
  BBands_xts<-calc_BBands(ticker, px_type, window_size, sd, maType, ratio)
  return (BBands_xts$pctB)
}

#' calc_Volume_ROC
#'
#' @param ticker 
#' @param period_size 
#'
#' @return
#' @export
#'
#' @examples
calc_Volume_ROC<-function(ticker, period_size = 1){
  xts_obj<-get(ticker)
  volume<-Vo(xts_obj)
  volume_ROC<-ROC(volume, n = period_size, type = c("discrete"))
  colnames(volume_ROC)<-paste(ticker, "VROC", sep = ".")
  return(volume_ROC)
}

#' calc_williamsAD
#'
#' @param ticker 
#'
#' @return
#' @export
#'
#' @examples
calc_williamsAD<-function(ticker){
  xts_obj<-get(ticker)
  px_series<-HLC(xts_obj)
  WAD<-williamsAD(px_series)
  adj_cl_px_series<-Ad(xts_obj)
  returned_xts<-merge(adj_cl_px_series, WAD)
  return(returned_xts)
}

#' calc_and_use_williamsAD
#'
#' @param ticker 
#' @param run_win 
#'
#' @return
#' @export
#'
#' @examples
calc_and_use_williamsAD<-function(ticker, run_win = 21){
  xts_obj<-get(ticker)
  px_series<-HLC(xts_obj)
  WAD<-williamsAD(px_series)
  adj_cl_px_series<-Ad(xts_obj)
  # calc run max and mins
  running_max_WAD<-runMax(WAD, n = run_win)
  running_min_WAD<-runMin(WAD, n = run_win)
  running_max_adj_cl<-runMax(adj_cl_px_series, n = run_win)
  running_min_adj_cl<-runMin(adj_cl_px_series, n = run_win)
  running_max_hi<-runMax(Hi(xts_obj), n = run_win)
  running_min_lo<-runMin(Lo(xts_obj), n = run_win)
  # calc momentums
  max_adj_cl_change<-momentum(running_max_adj_cl)
  min_adj_cl_change<-momentum(running_min_adj_cl)
  hi_px_change<-momentum(running_max_hi)
  lo_px_change<-momentum(running_min_lo)
  max_WAD_change<-momentum(running_max_WAD)
  min_WAD_change<-momentum(running_min_WAD)
  # filter momentums
  filtered_max_adj_cl_change<-ifelse(max_adj_cl_change <= 0, 0.0, max_adj_cl_change)
  filtered_min_adj_cl_change<-ifelse(min_adj_cl_change >= 0, 0.0, min_adj_cl_change)
  filtered_hi_px_change<-ifelse(hi_px_change <= 0, 0.0, hi_px_change)
  filtered_lo_px_change<-ifelse(lo_px_change >= 0, 0.0, lo_px_change)
  filtered_max_WAD_change<-ifelse(max_WAD_change <= 0, 0.0, max_WAD_change)
  filtered_min_WAD_change<-ifelse(min_WAD_change >= 0, 0.0, min_WAD_change)
  # merge data
  y<-merge(filtered_max_adj_cl_change, filtered_min_adj_cl_change)
  y<-merge(y, filtered_hi_px_change)
  y<-merge(y, filtered_lo_px_change)
  y<-merge(y, filtered_max_WAD_change)
  y<-merge(y, filtered_min_WAD_change)
  y<-merge(y, Ad(xts_obj))
  colnames(y)<-c(paste(ticker, "New_Cl_Hi", sep = "."), paste(ticker, "New_Cl_Lo", sep = "."), paste(ticker, "New_Hi", sep = "."),
                 paste(ticker, "New_Lo", sep = "."), paste(ticker, "New_WAD_Hi", sep = "."), paste(ticker, "New_WAD_Lo", sep = "."),
                 paste(ticker, "Adjusted", sep = "."))
  return(y)
}


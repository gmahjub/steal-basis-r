#' calc_period_Median_price
#'
#' Calculate the median price of a specific period, of size
#' num_periods-period_type. Meaning if period_type = weeks and num_periods is 2,
#' then the period to calculate the median price for is 2 weeks, and a new value
#' will be calculated every 2 weeks. This is NOT a rolling calculaion.
#'
#' @param ticker
#' @param px_type
#' @param period_type
#' @param num_periods
#'
#' @return a single column xts with the median labeled
#'   <ticker>.<num_periods>.<period_type>.Median
#' @export
#'
#' @examples
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

#' calc_rolling_Median_price
#'
#' Calculate median on a rolling basis, with the size of the window specified by
#' window_size. Note that the window size is not a number of days or weeks or
#' anything like that. It is simply the number of periods, where periods is
#' whatever perciodicity the data is in.
#'
#' @param ticker
#' @param px_type
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
calc_rolling_Median_price<-function(ticker, px_type = "A", window_size = 5){
  xts_obj<-get(ticker)
  px_series<-Ad(xts_obj)
  if (px_type == "O"){ px_series <- Op(xts_obj) }
  else if (px_type == "H") { px_series <- Hi(xts_obj) }
  else if (px_type == "L") { px_series <- Lo(xts_obj) }
  else if (px_type == "C") { px_series <- Cl(xts_obj) }
  median_px_series<-runMedian(px_series, n = window_size)
  colnames(median_px_series)<-paste(ticker, "Rolling", window_size, "PeriodMedian", sep = ".")
  return (median_px_series)
}

#' calc_period_Highest_High_price
#'
#' Return the highest price of a specified period, where the highest price is
#' the Highest High
#'
#' @param ticker
#' @param period_type
#' @param num_periods
#'
#' @return
#' @export
#'
#' @examples
calc_period_Highest_High_price<-function(ticker, period_type = 'weeks', num_periods=1){
  xts_obj<-get(ticker)
  px_series<-Hi(xts_obj)
  ep <- endpoints(px_series, period_type, k = num_periods)
  period_highest_high <- period.apply(px_series, INDEX = ep, FUN = max )
  colnames(period_highest_high) <- paste(ticker, num_periods, period_type, "Highest_High", sep = ".")
  return(period_highest_high)
}

#' calc_rolling_Highest_High_price
#'
#' @param ticker 
#' @param window_size 
#'
#' @return
#' @export
#'
#' @examples
calc_rolling_Highest_High_price<-function(ticker, window_size = 5){
  xts_obj<-get(ticker)
  px_series<-Hi(xts_obj)
  highest_high_px_series<-runMax(px_series, n = window_size)
  colnames(highest_high_px_series) <- paste(ticker, window_size, "Rolling_High", sep = ".")
  return(highest_high_px_series)
}

#' calc_period_Lowest_Low_price
#' 
#' Return an xts object with the lowest low of a specified period.
#'
#' @param ticker 
#' @param period_type 
#' @param num_periods 
#'
#' @return
#' @export
#'
#' @examples
calc_period_Lowest_Low_price<-function(ticker, period_type = 'weeks', num_periods = 1){
  xts_obj <- get(ticker)
  px_series<-Lo(xts_obj)
  ep <- endpoints(px_series, period_type, k = num_periods)
  period_lowest_low <- period.apply(px_series, INDEX = ep, FUN = min)
  colnames(period_lowest_low) <- paste(ticker, num_periods, period_type, "Lowest_Low", sep = ".")
  return (period_lowest_low)
}

#' calc_rolling_Lowest_Low_price
#'
#' Rolling lowest low where the period size is a rolling window of size
#' window_size.
#'
#' @param ticker
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
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
calc_BBands<-function(ticker, px_type = "HLC", window_size = 20, sd = 2, maType = EMA, ratio = NULL){
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

#' calc_PercentB
#' 
#' Utilizes the Bolinger Bands function (BBands)
#' Return PercentB from Bolinger Bands calculation.
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
calc_PercentB<-function(ticker, px_type = "HLC", window_size = 20, sd = 2, maType = EMA, ratio = NULL){
  BBands_xts<-calc_BBands(ticker, px_type, window_size, sd, maType, ratio)
  return (BBands_xts$pctB)
}

#' calc_Bandwidth
#' 
#' Utilizes the BBands function.
#'
#' @param ticker 
#' @param px_type 
#' @param window_size 
#' @param sd 
#' @param maTYpe 
#' @param ratio 
#'
#' @return
#' @export
#'
#' @examples
calc_Bandwidth<-function(ticker, px_type = "HLC", window_size = 20, sd = 2, maTYpe = EMA, ratio = NULL){
  BBands_xts <- calc_BBands(ticker, px_type, window_size, sd, maType, ratio)
  BBands_xts<-na.omit(BBands_xts)
  bandwidth <- BBands_xts$up - BBands_xts$dn
  return(bandwidth)
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

#' calc_Px_ROC
#'
#' @param ticker 
#' @param px_type 
#' @param period_size 
#'
#' @return
#' @export
#'
#' @examples
calc_Px_ROC<-function(ticker, px_type = "A", period_size = 1){
  xts_obj <- get(ticker)
  px_series <-Ad(xts_obj)
  if (px_type == "O"){
    px_series <- Op(xts_obj)
  } else if (px_type == "H"){
    px_series <- Hi(xts_obj)
  } else if (px_type == "L"){
    px_series <- Lo(xts_obj)
  } else if (px_type == "C"){
    px_series <- Cl(xts_obj)
  }
  price_ROC<-ROC(px_series, n = period_size, type = C("discrete"))
  colnames(price_ROC)<-paste(ticker, "PxROC", sep = ".")
  return (price_ROC)
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
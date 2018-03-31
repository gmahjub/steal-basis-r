#' calc_stochastics
#'
#' the most important arguement here is the maType arguement. The maType
#' argument is a list of moving average types. They are as follows: SMA,
#' list(EMA, wilder = FALSE, ratio = NULL), DEMA, WMA, EVWMA, VMA, ZLEMA, etc..
#' You can see all the possible arguements for maType in ?EMA for example.
#' Stochastics requires 3 moving averages, and so you must pass a list of 3
#' lists to maType. See further notes inside the function. Furthermore, this
#' funciton is called by the individual stochastic type function below it, such
#' as calc_fast_stochastics_percentK(), calc_fast_stochastics_percentD(), etc...
#' Those functions simply return a specific column of the returned xts from this
#' function.
#'
#' @param ticker
#' @param nFastK
#' @param nFastD
#' @param nSlowD
#' @param maType
#' @param bounded
#' @param smooth inteer, number of periods of internal smoothing to apply to the
#'   differences in the HLC range b4 calculating Fast K
#'
#' @return
#' @export
#'
#' @examples
calc_stochastics<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType=list(list(EMA, wilder = TRUE), list(SMA), list(SMA)), 
                           bounded = TRUE, smooth = 1){
  # fast %D = slow %K - they are equal, that's why we don't see a return value for slow %K
  # maType here must specify 3 types of MA's, the first for the FastD, the second for the SlowD, and the third for the internal smoothing
  #  of the C-L and H-L values to calculated FastK.
  xts_obj<-get(ticker)
  hlc_px_series<-HLC(xts_obj)
  stoch_vals<-stoch(hlc_px_series, nFastK = nFastK, nFastD = nFastD, nSlowD = nSlowD, maType = maType, bounded = bounded, smooth = smooth)
  return (stoch_vals)
}

#' calc_fast_stochastics_percentK
#' 
#' Return percentK fast stochastic column from calc_stochastics xts return value.
#'
#' @param ticker 
#' @param nFastK 
#' @param nFastD 
#' @param nSlowD 
#' @param maType 
#' @param bounded 
#' @param smooth 
#'
#' @return
#' @export
#'
#' @examples
calc_fast_stochastics_percentK<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = FALSE)),
                                         bounded = TRUE, smooth = 1){
  # TTR::stoch()
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastK)
}

calc_fast_stochastics_percentD<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = FALSE)),
                                         bounded = TRUE, smooth = 1){
  # TTR:stoch()
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastD)
}

calc_slow_stochastics_percentK<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = FALSE)),
                                         bounded = TRUE, smooth = 1){
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastD)
}

calc_slow_stochastics_percentD<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = FALSE)),
                                         bounded = TRUE, smooth = 1){
  # TTR:stoch()
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$slowD)
}


#' Williams %R
#' 
#' Williams %R is the inverse of the Fast Stochastic Oscillator (Fast %K) 
#'
#' @param ticker 
#'
#' @return
#' @export
#'
#' @examples
calc_williams_percentR<-function(ticker, nFastK = 14, nFastD = 3, maType = EMA, ratio = NULL, wilder = FALSE){
  xts_obj<-get(ticker)
  stoch_vals <- calc_stochastics(ticker, nFastK, nFastD, 3)
  williamsPercentR<-(1-stoch_vals$fastK)*-100.0
  ema_williamsPercentR<-do.call(eval(maType), list(williamsPercentR, n=nFastD))
  return_xts<-merge(williamsPercentR, ema_williamsPercentR)
  return_xts<-merge(return_xts, Ad(xts_obj))
  colnames(return_xts)<- c(paste(ticker, "Williams%R", sep = "."), paste(ticker, "SmoothedWilliams%R", sep = "."), names(Ad(xts_obj)))
  return (return_xts)
}

#' calc_chaikin_volatility
#'
#' NOt sure if we want to use this as is or we want to modify it. This is
#' measuring volatility, specifically, the change in range between time t and
#' time t-window_size. There is a limit to how negative this can go, but much
#' less of a limit as to how big it can get since volatility can grow infinitely
#' but is limited to 0 on the down side.
#'
#' @param ticker
#' @param window_size
#' @param maType
#' @param ratio
#' @param wilder
#'
#' @return
#' @export
#'
#' @examples
calc_chaikin_volatility<-function(ticker, window_size=6, maType = EMA, ratio = NULL, wilder = FALSE){
  xts_obj<-get(ticker)
  hlc_px_series<-HLC(xts_obj)
  hl_px_series<-hlc_px_series[,1:2]
  cv_xts<-chaikinVolatility(hl_px_series, n = window_size, maType = maType, ratio = ratio, wilder = wilder)
  return_xts<-merge(cv_xts, Ad(xts_obj))
  return(return_xts)
}

calc_volatility<-function(ticker){
  xts_obj<-get(ticker)
}

#' calc_stoch_of_RSI
#'
#' Calculate stochastics of RSI signal. Must calculate the RSI signal seperately
#' and send as input to this funciton.
#'
#' @param rsi_xts
#' @param nFastK
#' @param nFastD
#' @param nSlowD
#' @param maType
#' @param bounded
#' @param smooth
#'
#' @return
#' @export
#'
#' @examples
calc_stoch_of_RSI<-function(rsi_xts, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = TRUE)),
                            bounded = TRUE, smooth = 1){
  stoch_vals<-stoch(rsi_xts, nFastK = nFastK, nFastD = nFastD, nSlowD = nSlowD, maType = maType, bounded = bounded, smooth = smooth)
  return (stoch_vals)
}

#' calc_SMI
#' 
#' Stochastic momentum index
#'
#' @param ticker 
#' @param n 
#' @param nFast 
#' @param nSlow 
#' @param nSig 
#' @param maType 
#' @param bounded 
#'
#' @return
#' @export
#'
#' @examples
calc_SMI<-function(ticker, n = 13, nFast = 2, nSlow = 25, nSig = 9, 
                   maType = list(list(EMA, wilder = FALSE), list(EMA, wilder = FALSE), list(EMA, wilder = FALSE)), 
                   bounded = TRUE){
  xts_obj<-get(ticker)
  hlc_px_series<-HLC(xts_obj)
  smi_vals<-SMI(hlc_px_series, nFast = nFast, nSlow = nSlow, nSig = nSig, maType = maType, bounded = bounded)
  adCl_px_series<-Ad(xts_obj)
  return_xts<-merge(smi_vals, adCl_px_series)
  smi_minus_signal<-smi_vals$SMI - smi_vals$signal
  return_xts<-merge(return_xts, smi_minus_signal)
  colnames(return_xts)<-c(paste(ticker, "SMI", sep = "."), paste(ticker, "signal", sep = "."), names(adCl_px_series), 
                          paste(ticker, "SMI-Signal", sep = "."))
  return(return_xts)
}

#' calc_period_historical_volatility
#'
#' We are calculating the historical volatility of the data as is passed into
#' the function. So if the data is daily data, and the period type is "weeks",
#' the we will return the volatility of daily returns, for every "num_periods"
#' weeks period size. For example, if num_periods = =6, and period_tyhpe is
#' "weeks", the we will return the volatility (sd) of daily returns calculated
#' every 6 weeks. If you want the volatility of weekly returns, then you must
#' pass in data that is weekly data.
#'
#' @param ticker
#' @param px_type
#' @param period_type
#' @param num_periods
#' @param down_sample 
#' @param down_sample_to 
#' @param down_sample_k 
#'
#' @return
#' @export
#'
#' @examples
calc_period_historical_volatility<-function(ticker, px_type = "A", period_type = "weeks", 
                                            num_periods = 1, down_sample = FALSE, 
                                            down_sample_to=NULL, down_sample_k = 1){
  xts_obj<-get(ticker)
  if (down_sample){
    if (is.null(down_sample_to)){
      stop("If down_sample is TRUE, down_sample_to cannot be NULL, must specify one of: 
           days, weeks, months, etc..., of lower periodicity than the input data")
    }
    # not sure if doing the downsample actually creates new High and Low...i.e. high of the week/month/quarter
    # or if it just takes the High/Low of the endpoint. TODO: confirm this and fix if neccessary.
    xts_obj<-to.period(xts_obj, period = period_type, k = down_sample_k  )
  }
  px_series<-Ad(xts_obj)
  if (px_type == "O"){
    px_series<-Op(xts_obj)
  } else if (px_type == "H"){
    px_series <- Hi(xts_obj)
  } else if (px_type == "L"){
    px_series <- Lo(xts_obj)
  } else if (px_type == "C'"){
    px_series<-Cl(xts_obj)
  } else if (px+type == "V"){
    px_series<-Vo(xts_obj)
  }
  rets_series_xts<-ROC(px_series, type = "continuous")
  rets_series_xts<-na.omit(rets_series_xts)
  ep<-endpoints(rets_series_xts, on = period_type, k = num_periods)
  cnt<-period.apply(rets_series_xts, INDEX = ep, FUN = length )
  sd_series<-period.apply(rets_series_xts, INDEX = ep, FUN = sd)
  hist_vol_series<-sd_series/sqrt(cnt)
  return (hist_vol_series)  
}

#' calc_rolling_historical_volatility
#'
#' takes the input data as is, and calculates a rolling historical volatility,
#' with a look back of <window_size> periods, whatever those periods may be. Not
#' periodicity changing is done in this function. You must pass in the data
#' exactly as you want it to be used.
#'
#' @param ticker
#' @param px_type
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
calc_rolling_historical_volatility<-function(ticker, px_type = "A", window_size = 5){
  xts_obj<-get(ticker)
  px_series<-Ad(xts_obj)
  if (px_type == "O"){
    px_series<-Op(xts_obj)
  } else if (px_type == "H"){
    px_series <- Hi(xts_obj)
  } else if (px_type == "L"){
    px_series <- Lo(xts_obj)
  } else if (px_type == "C'"){
    px_series<-Cl(xts_obj)
  } else if (px+type == "V"){
    px_series<-Vo(xts_obj)
  }
  rets_series_xts<-ROC(px_series, type = "continuous")
  rets_series_xts<-na.omit(rets_series_xts)
  rolling_sd_series<-rollapply(rets_series_xts, width = window_size, FUN = sd )
  return (rolling_sd_series)
}

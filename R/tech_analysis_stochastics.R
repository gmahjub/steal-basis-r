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

#' calc_fast_stochastics_percentD
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
calc_fast_stochastics_percentD<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = FALSE)),
                                         bounded = TRUE, smooth = 1){
  # TTR:stoch()
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastD)
}

#' calc_slow_stochastics_percentK
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
calc_slow_stochastics_percentK<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = FALSE)),
                                         bounded = TRUE, smooth = 1){
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastD)
}

#' calc_slow_stochastics_percerntD
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
calc_chaikin_volatility<-function(ticker, window_size=10, maType = EMA, ratio = NULL, wilder = FALSE){
  xts_obj<-get(ticker)
  hlc_px_series<-HLC(xts_obj)
  hl_px_series<-hlc_px_series[,1:2]
  cv_xts<-chaikinVolatility(hl_px_series)# n = window_size, maType = maType, ratio = ratio, wilder = wilder)
  return_xts<-merge(cv_xts, Ad(xts_obj))
  return(return_xts)
}

#' calc_volatility
#' 
#' This funciton is an implementation of the volatility function from TTR
#'
#' @param ticker 
#'
#' @return
#' @export
#'
#' @examples
calc_simple_close_to_close_volatility<-function(ticker, window_size = 10, period_type = 'days', periods_per_year = 250){
  xts_obj<-get(ticker)
  if (period_type == 'days'){
    periods_per_year <- 250
  } else if (period_type == 'weeks'){
    periods_per_year <- 52
  } else if (period_type == 'months'){
    periods_per_year<-12
  } else if (period_type == 'quarters'){
    periods_per_year<-4
  } else if (period_tyep == 'years'){
    periods_per_year<-1
  }
  close_to_close_vol <- volatility(xts_obj, n = window_size, calc = "close", N = periods_per_year)
  return(close_to_close_vol)
}

#' calc_OHLC_volatility
#' 
#' Using Garman and Klass estimator
#'
#' @param ticker 
#' @param window_size 
#' @param period_type 
#' @param periods_per_year 
#'
#' @return
#' @export
#'
#' @examples
calc_OHLC_volatility<-function(ticker, window_size= 10, period_type = 'days', periods_per_year = 250){
  xts_obj<-get(ticker)  
}

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

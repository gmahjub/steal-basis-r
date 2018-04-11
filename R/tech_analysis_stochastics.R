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
calc_chaikin_volatility<-function(ticker, window_size=10, maType = EMA, ratio = NULL, wilder = FALSE){
  xts_obj<-get(ticker)
  hlc_px_series<-HLC(xts_obj)
  hl_px_series<-hlc_px_series[,1:2]
  cv_xts<-chaikinVolatility(hl_px_series, n = window_size, maType = maType, ratio = ratio, wilder = wilder)
  return_xts<-merge(cv_xts, Ad(xts_obj))
  colnames(return_xts)<-c(paste(ticker, "EMA_Chaikin_Vol", sep = "."), names(Ad(xts_obj)))
  return(return_xts)
}

plotting_chaikin_volatility<-function(ticker, chaikin_vol_xts, chaikin_vol_upper_th = 0.75, chaikin_vol_lower_th = -0.25, plot_which_event = "ALL"){
  the_ema_ts<-chaikin_vol_xts[, paste(ticker, "EMA_Chaikin_Vol", sep = ".")]
  adj_px_ts<-chaikin_vol_xts[, paste(ticker, "Adjusted", sep = ".")]
  true_dates_upper_bound<-the_ema_ts > chaikin_vol_upper_th
  true_dates_lower_bound <- the_ema_ts < chaikin_vol_lower_th
  true_dates_upper_bound<-na.omit(true_dates_upper_bound)
  true_dates_lower_bound<-na.omit(true_dates_lower_bound)
  upper_bnd_events_xts<-true_dates_upper_bound[true_dates_upper_bound[, paste(ticker, "EMA_Chaikin_Vol", sep = ".")] == TRUE]
  col_name_upper<-paste(ticker, "Chai.Vol.Event", sep = ".")
  colnames(upper_bnd_events_xts) <- c(col_name_upper)
  lower_bnd_events_xts<-true_dates_lower_bound[true_dates_lower_bound[, paste(ticker, "EMA_Chaikin_Vol", sep = ".")] == TRUE]
  col_name_lower <- c(paste(ticker, "Chai.Vol.Event", sep = "."))
  colnames(lower_bnd_events_xts) <- c(col_name_lower)
  upper_bnd_events_xts[,1] <- chaikin_vol_upper_th
  lower_bnd_events_xts[,1] <- chaikin_vol_lower_th
  if (plot_which_event == "ALL"){
    all_events<-rbind(upper_bnd_events_xts, lower_bnd_events_xts)
  } else if (plot_which_event == "LOWER"){
    all_events<-lower_bnd_events_xts
  } else if (plot_which_event == "UPPER"){
    all_events<-upper_bnd_events_xts
  }
  par(mfrow=c(1,1))
  plot(adj_px_ts, plot.type = "m", at="pretty")
  addEventLines(all_events, srt = 90, pos = 2, on = 1)
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
calc_simple_close_to_close_volatility<-function(ticker, window_size = 10, period_type = 'days', periods_per_year = 252){
  xts_obj<-get(ticker)
  if (period_type == 'days'){
    periods_per_year <- 252
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

#' calc_garman_klass_volatility
#'
#' Using Garman and Klass estimator. This estimation of volatility DOES NOT at
#' all take into account the O/N risk, where there are dramatic price changes
#' between prior close and the next open. Therefore, we should only use this
#' volatility estimation for trades that are purely day trades.
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
calc_garman_klass_volatility<-function(ticker, window_size= 10, period_type = 'days', periods_per_year = 252, mean0 = FALSE){
  xts_obj<-get(ticker)
  if (period_type == 'days'){
    periods_per_year <- 252
  } else if (period_type == 'weeks'){
    periods_per_year <- 52
  } else if (period_type == 'months'){
    periods_per_year<-12
  } else if (period_type == 'quarters'){
    periods_per_year<-4
  } else if (period_type == 'years'){
    periods_per_year<-1
  }
  garman_klass_vol<-volatility(xts_obj, n = window_size, calc = "garman.klass", N = periods_per_year, mean0 = mean0 )
  return (garman_klass_vol)
}

#' calc_gapping_garman_klass_volatility
#'
#' Garman-Klass Yang-Zhang volatility estimator. Basically the Garman Klass
#' calculation that allows for opening gaps. This estimation can be used for
#' multi-period estimation, specifically close at time t-1 to open at time t.
#'
#' @param ticker
#' @param window_size
#' @param period_type
#' @param periods_per_year
#' @param mean0
#'
#' @return
#' @export
#'
#' @examples
calc_gapping_garman_klass_volatility<-function(ticker, window_size = 10, period_type = 'days', periods_per_year = 260, mean0 = FALSE){
  xts_obj<-get(ticker)
  if (period_type == 'days'){
    periods_per_year<-260
  } else if (period_type == 'weeks'){
    periods_per_year<-52
  } else if (period_type == 'months'){
    periods_per_year<-12
  } else if (period_type == 'quarters'){
    periods_per_year<-4
  } else if (period_type == 'years'){
    periods_per_year <- 1
  }
  g_garman_klass_vol<-volatility(xts_obj, n = window_size, calc = "gk.yz", N = periods_per_year, mean0 = mean0)
  return (g_garman_klass_vol)
}

calc_yang_zhang_volatility<-function(ticker, window_size = 10, period_type = 'days', periods_per_year = 252, mean0 = FALSE){
  xts_obj<-get(ticker)
  if (period_type == 'days'){
    periods_per_year<-252
  } else if ()
}

#' calc_HL_volatility
#'
#' Uisng parkinson volatility estimator. This estimation of vol is based on
#' period High and Low prices. This estiamtion is basically exaclty the same as
#' close to close historical volatility, except instead of close to close, we
#' are taking daily High annd Low returns to calculate the volatility.
#'
#' @param ticker
#' @param window_size
#' @param period_type
#' @param periods_per_year
#' @param mean0
#'
#' @return
#' @export
#'
#' @examples
calc_HL_volatility<-function(ticker, window_size = 10, period_type = 'days', periods_per_year = 252, mean0 = FALSE){
  xts_obj<-get(ticker)
  if (period_type == 'days'){
    periods_per_year<-252
  } else if (period_type == 'weeks'){
    periods_per_year <- 52
  } else if (period_type == 'months'){
    periods_per_year <-12
  } else if (period_type == 'quarters'){
    periods_per_year <- 4
  } else if (period_type == 'years'){
    periods_per_year<-1
  }
  parkinson_vol<-volatility(xts_obj, n = window_size, calc = 'parkinson', N = periods_per_year, mean0 = mean0)
  return (parkinson_vol)
}

#' calc_rogers_satchell_vol_estimation
#'
#' Use this volatility estimator for intra-day brownian motion. This estimator
#' includes accountability for trending price, meaning the underlying has some
#' sort of price drift. It DOES NOT account for O/N gaps, it only takes into
#' considering OHLC prices per period. When we say underlying drift, we mean the
#' data has a non-zero mean return is non- zero.
#'
#' @param ticker
#' @param window_size
#' @param period_type
#' @param periods_per_year
#' @param mean0
#'
#' @return
#' @export
#'
#' @examples
calc_rogers_satchell_vol_estimation<-function(ticker, window_size = 10, period_type = 'days', periods_per_year = 252, mean0 = FALSE){
  xts_obj<-get(ticker)
  if (period_type == 'days'){
    periods_per_year<-252
  } else if (period_type == 'weeks'){
    periods_per_year <- 52
  } else if (period_type == 'months'){
    periods_per_year <- 12
  } else if (period_type == 'quarters'){
    periods_per_year <- 4
  } else if (period_type == 'years'){
    periods_per_year <- 1
  }
  rogers_satchell_vol<-volatility(xts_obj,4 n = window_size,  calc = 'rogers.satchell', N = periods_per_year, mean0 = mean0)
  return (rogers_satchell_vol)
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

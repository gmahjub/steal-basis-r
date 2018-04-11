#' calc_MACD
#' 
#' Calculate the Moverage Average Convergence Divergence for a time series.
#'
#' @param ticker 
#' @param px_type 
#' @param num_periods_fast 
#' @param num_periods_slow 
#' @param num_periods_signal 
#' @param maType 
#' @param percent 
#'
#' @return
#' @export
#'
#' @examples
calc_MACD<-function(ticker, px_type, num_periods_fast=12, num_periods_slow=26, num_periods_signal=9, 
                    maType=list(list(EMA), list(EMA), list(EMA)), percent = TRUE ) {
  xts_obj<-get(ticker)
  px_ts<-Ad(xts_obj)
  if (px_type == "O"){px_ts <- Op(xts_obj)
  } else if (px_type == "H"){ px_ts <- Hi(xts_obj)
  } else if (px_type == "L"){ px_ts <- Lo(xts_obj)
  } else if (px_type == "C"){ px_ts <- Cl(xts_obj)
  } else if (px_type == "A"){ px_ts <- Ad(xts_obj)
  } else if (px_type == "V"){ px_ts <- Vo(xts_obj)
  } else {
    stop( paste("Invalid px_type: ", px_type, " must be one of O, H, L, C, A, V ", sep = " " ))
  }
  macd_ind<-MACD(px_ts, nFast = num_periods_fast, nSlow = num_periods_slow, nSignal = num_periods_signal, maType = maType, percent = percent)
  return(macd_ind)
}

#' calc_accumulation_distribution_line
#' 
#' ChaikinAD
#'
#' @param ticker 
#' @param window_size 
#'
#' @return
#' @export
#'
#' @examples
calc_accumulation_distribution_line<-function(ticker, window_size = 21){
  xts_obj<-get(ticker)
  px_series<-HLC(xts_obj)
  adl<-((Cl(xts_obj) - Lo(xts_obj) - (Hi(xts_obj) - Cl(xts_obj)))/(Hi(xts_obj) - Lo(xts_obj)))*Vo(xts_obj)
  rolling_sum_adl<-rollapply(adl, width = window_size, FUN = sum )
  #return_series<-rollapply(px_series, width = window_size, FUN = chaikinAD, volume = Vo(xts_obj))
  return_series<-merge(adl, rolling_sum_adl)
  return_series<-merge(return_series, Ad(xts_obj))
  colnames(return_series)<-c("ADL", "Rolling Cumm Adl", names(Ad(xts_obj)))
  return (return_series)  
}

#' calc_accumulation_distribution_oscillator
#' 
#' This is also known as a Daily Raw Figure. The maximum value is 100 and the minimum value is 0.
#' The value is normalied to the range. The maximum value is reached when the market opens trading at
#' the low and closes trading at the high. When the market opens at the high and closes at the low,
#' the minimum value of 0 is reached. The day to day use of this indicator may require some sort of 
#' smoothing as it is very erratic. Week to week or montly periodicity may not, is it up to the user
#' to figure this out. Smoothing this value of out and drawing two lines, one at the top representing
#' overbought conditions and one at the bottom representing over-sold conditions.
#'
#' @param smoothing TRUE/FALSE, use smoothing or not.
#' @param maType default EMA, can be any one of WMA, SMA, EMA, etc... help(EMA) for list.
#' @param window_size default is 6, representing a ~ 30% smoothing constant.
#' @param ticker ticker string.
#'
#' @return
#' @export
#'
#' @examples
calc_accumulation_distribution_oscillator<-function(ticker, smoothing = FALSE, maType = EMA, window_size = 6 ){
  xts_obj<-get(ticker)
  ado_num<- Hi(xts_obj) - Op(xts_obj) + Cl(xts_obj) - Lo(xts_obj)
  ado_den<-2*(Hi(xts_obj) - Lo(xts_obj))
  ado<-100.0*ado_num/ado_den
  colnames(ado) <- c(paste(ticker, "ADO", sep = "."))
  if (smoothing){
    smoothed_ado<-do.call(maType, list(ado, n=window_size))
    ado<-merge(ado, smoothed_ado)
    colnames(ado)<-c(names(ado)[1], paste(ticker, window_size, "Per.EMA.ADO.", sep = "."))
  }
  return(ado)
}

#' calc_momentum_oscillator
#'
#' Simply, the ratio of some price (Op, Hi, Lo, Cl) at time t, to that same
#' price type at time t - time_lag. The time_lag here is some amount of time
#' in the rear, and it is in number of periods. Therefore, exactly how much
#' time it is depends on the periodicity of the inputted data. There is no
#' changing of periodicity going on here.
#'
#' @param time_lag 
#' @param ticker
#'
#' @return
#' @export
#'
#' @examples
calc_momentum_oscillator<-function(ticker, px_type = "A", time_lag=5){
  xts_obj<-get(ticker)
  px_series <- Ad(xts_obj)
  if (px_type == "O"){
    px_series<- Op(xts_obj)
  } else if (px_type == "H"){
    px_series <- Hi(xts_obj)
  } else if (px_type == "L"){
    px_series <- Lo(xts_obj)
  } else if (px_type == "C"){
    px_series <- Cl(xts_obj)
  }
  mom_rat<-px_series/lag.xts(px_series, k = time_lag)
  colnames(mom_rat)<-c(paste(ticker, time_lag, "Per.Lag.MomOsc", sep = "." ))
  adj_px_series<-Ad(xts_obj)
  mom_rat<-merge(mom_rat, adj_px_series)
  return(mom_rat)
}

calc_mom_accel_oscillator<-function(ticker, px_type = "A", time_lag = 5, N = 1){
  xts_obj<-get(ticker)
  px_series<-Ad(xts_obj)
  if (px_type == "O"){
    px_series<-Op(xts_obj)
  } else if (px_type == "H"){
    px_series <- Hi(xts_obj)
  } else if (px_type == "L"){
    px_series <- Lo(xts_obj)
  } else if (px_type == "C"){
    px_series <- Cl(xts_obj)
  }
  mom_rat<-calc_momentum_oscillator(ticker, px_type = px_type, time_lag = time_lag)
  acceleration_MOM_ind<-ROC(mom_rat[,1])*100.0
  return(acceleration_MOM_ind)
}

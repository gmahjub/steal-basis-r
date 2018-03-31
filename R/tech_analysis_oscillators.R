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

calc_accumulation_distribution_oscillator<-function(ticker, maSlow = list("EMA", n =10), maFast = list("EMA", n=3), percent=FALSE){
  xts_obj<-get(ticker)
  hlc_px_series<-HLC(xts_obj)
  volume_series<-Vo(xts_obj)
  chaikinOscill
}
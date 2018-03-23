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
  px_ts<-NA
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
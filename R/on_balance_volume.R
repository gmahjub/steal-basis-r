#' calc_rolling_obv
#'
#' Calculate a roling on-balance volume, over the window size specified.
#'
#' @param ticker a string, ticker name, representing the XTS object in memory to
#'   run the OBV function on.
#' @param win_size an integer, representing the window size to calculate OBV
#'   over.
#'
#' @return an xts obj, with a colume named <ticker>.<win_size>.rolling_obv
#' @export
#'
#' @examples
calc_rolling_obv<-function(ticker, win_size = 20){
  y<-create_obv_data(ticker)
  rolling_obv <- rollapply(y, width = win_size, FUN = sum)
  colnames(rolling_obv)<-paste(ticker, win_size, "rolling_obv", sep = ".")
  return(rolling_obv)
}

#' create_obv_data
#'
#' Helper function which prepares the volume column for OBV calculation.
#'
#' @param ticker
#'
#' @return an xts object, single column, labeled <ticker>.volume_ob_adj
#' @export
#'
#' @examples
create_obv_data<-function(ticker){
  xts_obj<-get(ticker)
  diff_col<-diff(Ad(xts_obj))
  colnames(diff_col)<- paste(ticker, "Px_Chg_Adj_Cl", sep = ".")
  volume_col<-Vo(xts_obj)
  y<-ifelse(diff_col > 0, volume_col, volume_col*-1.0)
  colnames(y)<-paste(ticker, "volume_ob_adj", sep = ".")
  return(y)
}

#' calc_period_obv
#'
#' Calculate the on-balance volume for a specific period. The period could be a
#' week, a month, a year, a quarter, etc... Returned is a xts object with new
#' periodicity as per the period_type. Can also use the num_periods param to
#' specify multiple periods (i.e. 2 weeks, 3 weeks, 2 quarters, etc...)
#'
#' @param ticker a string, representing the ticker name, of an Xts object in
#'   memory
#' @param period_type options are 'days', 'weeks', 'months', 'quarters', and
#'   'years'. The lowest periodicty supported is daily.
#' @param num_periods integer, number of period_types
#'
#' @return an xts obj, single column, named <ticker>.<num_periods>.<period_type>.OBV
#' @export
#'
#' @examples
calc_period_obv<-function(ticker, period_type = 'weeks', num_periods = 1){
  xts_obj<-get(ticker)
  y<-create_obv_data(ticker)
  ep <- endpoints(xts_obj, period_type, k = num_periods)
  period_obv<-period.apply(y, INDEX = ep, FUN = sum)
  col_names(period_obv)<-paste(ticker, num_periods, period_type, "OBV", sep = ".")
  return(period_obv)
}

#' calc_moving_volume_avg
#' 
#' Calculate a rolling moving average of volume.
#'
#' @param ticker a string, representing an xts object in memory by this ticker name.
#' @param win_size the rolling window size.
#'
#' @return an xts obj, single column, named <ticker>.<win_size>.Rolling.Avg.Volume
#' @export
#'
#' @examples
calc_moving_volume_avg<-function(ticker, win_size = 21){
  xts_obj<-get(ticker)
  volume_col<-Vo(xts_obj)
  rolling_avg_volume<-rollapply(volume_col, width = win_size, FUN  = mean)
  colnames(rolling_avg_volume)<-paste(ticker, win_size, "Rolling.Avg.Volume", sep = ".")
  return(rolling_avg_volume)
}

#' calc_period_volume_avg
#'
#' Calculate average volume for a specific period of some number of weeks,
#' months, quarters, years.
#'
#'
#' @param ticker a string, representing an xts object in memory, by this ticker name.
#' @param period_type options are "days", "weeks", "months", "quarters", "years".
#' @param num_periods an integer, number of periods, (e.g. 2 weeks, 4 months, 5 quarters, etc...)
#'
#' @return an xts object, single column, named <ticker>.<num_periods>.<period_type>.Avg_Volume
#' @export
#'
#' @examples
calc_period_volume_avg<-function(ticker, period_type = 'weeks', num_periods = 1){
  xts_obj<-get(ticker)
  volume_col<-Vo(xts_obj)
  ep <- endpoints(xts_obj, period_type, k = num_periods)
  period_avg_volume<-period.apply(volume_col, INDEX = ep, FUN = mean)
  colnames(period_avg_volume)<-paste(ticker, num_periods, period_type, "Avg_Volume", sep = ".")
  return (period_avg_volume)
}
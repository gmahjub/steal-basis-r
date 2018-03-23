# Functions in this file extend upon quantmod and implement function using
# quantmod library function. The following several function take existing xts
# objects retrieved with getSymbol and add columns, such as return price over
# price of each in OHLC.

#' calc_Close_to_Close_returns
#'
#' calculates the returns of the close at t and the close at t-1. Uses the
#' Return.calculate from PerformanceAnalytics functionality. Creates a new
#' column named "ClClDailyRets" and appends it onto existing xts object in
#' memory and assigns this new object to memory with name "ticker". Requires
#' that there be an object named ticker in memory, that contains a "Close"
#' column since we are also using QuantMod here. This function is part of a
#' workflow. You must have called getSymbols() before this function with the
#' auto.assign option set to TRUE.
#'
#' @param ticker
#'
#' @return returns nothing.
#' @export
#'
#' @examples
calc_Close_to_Close_returns <- function(ticker){
  the_dataframe <-get(ticker, envir = parent.frame())
  close_price_series<-Cl(the_dataframe)
  cl_cl_returns<-Return.calculate(close_price_series)
  cl_cl_returns<-cl_cl_returns[-1,]
  new_col_names<-c(names(the_dataframe), paste(ticker, ".","ClClRets", sep=''))
  the_dataframe$ClClRets<-cl_cl_returns
  names(the_dataframe)<-new_col_names
  assign(ticker, the_dataframe[-1,], envir = parent.frame())
}

#' calc_AdjCl_to_AdjCl_returns
#'
#' calculates the returns of the adjusted close at t and the adjusted close at t-1. Uses the
#' Return.calculate functionality. Creates a new column named "AdAdDailyRets"
#' and appends it onto existing xts object and assigns this new object to memory
#' with name "ticker".
#'
#' @param ticker the ticker string representing the xts time series data object
#'
#' @return
#' @export
#'
#' @examples
calc_AdjCl_to_AdjCl_returns <- function(ticker){
  the_dataframe<-get(ticker, envir=parent.frame())
  adj_close_price_series<-Ad(the_dataframe)
  adjCl_adjCl_returns<-Return.calculate(adj_close_price_series)
  adjCl_adjCl_returns<-adjCl_adjCl_returns[-1,]
  new_col_names<-c(names(the_dataframe), paste(ticker, ".","AdjClAdjClRets", sep=''))
  the_dataframe$AdjClAdjClRets<-adjCl_adjCl_returns
  names(the_dataframe)<-new_col_names
  assign(ticker, the_dataframe[-1,], envir = parent.frame())
}

#' calc_Open_to_Open_returns
#'
#' calculates the returns of the open at t and the open at t-1. Uses the
#' Return.calculate functionality. Creates a new column named "OpOpDailyRets"
#' and appends it onto existing xts object and assigns this new object to memory
#' with name "ticker".
#'
#' @param ticker the ticker string representing the xts time series data object
#'
#' @return returns nothing
#' @export
#'
#' @examples
calc_Open_to_Open_returns <- function(ticker){
  the_dataframe<- get(ticker, envir = parent.frame())
  open_price_series<-Op(the_dataframe)
  op_op_returns<-Return.calculate(open_price_series)
  op_op_returns<-op_op_returns[-1,]
  new_col_names<-c(names(the_dataframe), paste(ticker, ".","OpOpRets", sep=''))
  the_dataframe$OpOpRets<-op_op_returns
  names(the_dataframe)<-new_col_names
  assign(ticker, the_dataframe[-1,], envir = parent.frame())
}

#' calc_High_to_High_returns
#'
#' calculates the returns of the close at t and the close at t-1. Uses the
#' Return.calculate functionality. Creates a new column named "HiHiDailyRets"
#' and appends it onto existing xts object and assigns this new object to memory
#' with name "ticker".
#'
#' @param ticker the ticker string representing the xts time series data object
#'
#' @return returns nothing
#' @export
#'
#' @examples
calc_High_to_High_returns <- function(ticker){
  the_dataframe<-get(ticker, envir=parent.frame())
  high_price_series<-Hi(the_dataframe)
  hi_hi_returns<-Return.calculate(high_price_series)
  hi_hi_returns<-hi_hi_returns[-1,]
  new_col_names<-c(names(the_dataframe), paste(ticker, ".","HiHiRets", sep=''))
  the_dataframe$HiHiRets<-hi_hi_returns
  names(the_dataframe)<-new_col_names
  assign(ticker, the_dataframe[-1,], envir = parent.frame())
}

#' calc_Low_to_Low_returns
#'
#' calculates the returns of the close at t and the close at t-1. Uses the
#' Return.calculate functionality. Creates a new column named "LoLoDailyRets"
#' and appends it onto existing xts object and assigns this new object to memory
#' with name "ticker".
#'
#' @param ticker the ticker string representing the xts time series data object
#'
#' @return returns nothing
#' @export
#'
#' @examples
calc_Low_to_Low_returns<-function(ticker){
  the_dataframe<-get(ticker, envir = parent.frame())
  low_price_series<-Lo(the_dataframe)
  lo_lo_returns<-Return.calculate(low_price_series)
  lo_lo_returns<-lo_lo_returns[-1,]
  new_col_names<-c(names(the_dataframe), paste(ticker,".","LoLoDailyRets", sep=''))
  the_dataframe$DailyLoLoRets<-lo_lo_returns
  names(the_dataframe)<-new_col_names
  assign(ticker, the_dataframe[-1,], envir=parent.frame())
}

#' calc_change_in_volume
#'
#' Calculates change in volume, row over row. Extends quantmod. Uses
#' PerformanceAnalytics Return.calculate. Requires a call to getSymbol, with
#' auto.assign set to TRUE.
#'
#' @param ticker
#'
#' @return nothing
#' @export
#'
#' @examples
calc_change_in_volume<-function(ticker){
  the_dateframe<-get(ticker, envir=parent.frame())
  volume_series<-Vol(the_dateframe)
  volume_change<-Return.calculate(volume_series)
  volume_change<-volume_change[-1,]
  new_col_names<-c(names(the_dateframe), paste(ticker, ".", "VolumeChange", sep=''))
  the_dateframe$VolumeChange<-volume_change
  names(the_dateframe)<-new_col_names
  assign(ticker, the_dataframe[-1,], envir = parent.frame())
}

#' create_portfolio_xts
#'
#' creates a portfolio of xts tickers objects with only the given price type in
#' the mew merged xts object. The price type options are: "Hi", "Lo", "Op",
#' "Cl", and "Ad". Returned from this function is a new xts with multiple
#' tickers with the price_type as one colunn per ticker.
#'
#' @param tickers a list of tickers to create the portfolio xts obect
#' @param price_type the price type : "Hi", "Lo", "Op", "Cl", "Ad". Only
#'   price_type price for each ticker in tickers.
#'
#' @return a new xts object representing the portfolio of tickers with price type price.
#' @export
#'
#' @examples
create_portfolio_xts<-function(tickers, price_type="Ad"){
  # default to adjust close price for price type
  xts_objs_list <- lapply(tickers, get)
  if(price_type=="Ad"){xts_objs_list_price_type_col <- lapply(xts_objs_list, get_adjusted_close_column)}
  if(price_type=="Op"){xts_objs_list_price_type_col <- lapply(xts_objs_list, get_open_column)}
  if(price_type=="Hi"){xts_objs_list_price_type_col <- lapply(xts_objs_list, get_high_column)}
  if(price_type=="Lo"){xts_objs_list_price_type_col <- lapply(xts_objs_list, get_low_column)}
  if(price_type=="Cl"){xts_objs_list_price_type_col <- lapply(xts_objs_list, get_close_column)}
  names(xts_objs_list_price_type_col)<-tickers
  returned<-do.call(merge, xts_objs_list_price_type_col)
  return (returned)
}

#' create_weekly_max_min_mean_std_of_Ad_price
#'
#' uses endpoints() to split xts_obj into weekly timeseries and then, per weekly
#' timeseries, find the min, max, mean, sd, of the week price data. Returns a
#' new xts object with ticker.max, ticker.min, ticker.mean, and ticker.sd as the
#' columns.
#'
#' @param xts_obj an xts object as would be returned by
#'   getSymbol(<stock_ticker>). This function is very strict in its input and
#'   output and requires there be a "Adjusted" (adjusted close) price column.
#'
#' @return a new xts object with the weekly min, max, mean, and sd of the
#'   adjusted close price for inputted xts_obj
#' @export
#'
#' @examples
create_weekly_max_min_mean_std_of_Ad_price<-function(xts_obj){
  adjusted_close_price_xts_obj<-Ad(xts_obj)
  ep<-endpoints(adjusted_close_price_xts_obj, "weeks")
  the_weekly_Ad_max<-period.apply(adjusted_close_price_xts_obj, INDEX=ep, FUN=seriesHi)#max)
  the_weekly_Ad_min<-period.apply(adjusted_close_price_xts_obj, INDEX=ep, FUN=seriesLo)#min)
  the_weekly_Ad_mean<-period.apply(adjusted_close_price_xts_obj, INDEX=ep, FUN=mean)
  the_weekly_Ad_sd<-period.apply(adjusted_close_price_xts_obj, INDEX=ep, FUN=sd)
  list_frames_to_merge<-list(the_weekly_Ad_max, the_weekly_Ad_min, the_weekly_Ad_mean, the_weekly_Ad_sd)
  returned<-do.call(merge, list_frames_to_merge)
  ticker_name<-names(adjusted_close_price_xts_obj)
  names(returned)<-c(paste(ticker_name,".","Max", sep=''), paste(ticker_name,".", "Min", sep=''), paste(ticker_name,".","Mean", sep=''), paste(ticker_name, ".","Std",sep=''))
  return (returned)
}

#' create_xts_transform_of_price_type
#'
#' Generic form of create_weekly_max_min_mean_std_of_Ad_price() function. Time
#' window is generalized, options are: us, secs, mins, minutes, seconds, hours,
#' days, weeks, months, quarters, years. FUN_transform options are: max, min,
#' mean, sd, or any other xts transform function, defaulted to "mean". Function
#' will use endpoints() to split the data by time_window, and then will apply
#' the transform, and then will merge the xts back and return said xts.
#'
#'
#' @param xts_obj xts object containing time series data to be transformed.
#' @param time_window over what time window to transform the data. Usually the
#'   data will be daily so transforms to higher periodicity (e.g. minutes) is
#'   not possible.
#' @param FUN_price_type function to pull a specifc price data from the xts_obj:
#'   Ad(), Cl(), Op(), Hi(), Lo()
#' @param FUN_transform how to transform the data, what function to use, e.g.
#'   mean, min, max, sd, etc...
#'
#' @return a new xts object
#' @export
#'
#' @examples
create_xts_transform_of_price_type<-function(xts_obj, time_window, FUN_price_type, FUN_transform=mean){
  price_type_xts <- do.call(FUN_price_type, list(xts_obj))
  ep<-endpoints(price_type_xts, time_window)
  transform_applied_xts<-period.apply(price_type_xts, INDEX = ep, FUN=FUN_transform)
  col_name<- names(price_type_xts)
  names(transform_applied_xts)<-c(paste(col_name, ".", deparse(substitute(FUN_transform)), sep=''))
  return(transform_applied_xts)
}

#' merge_created_dataframe_with_original_dataframe
#'
#' This function takes two dataframes and will merge them together using all
#' default pararms for merge function. Will omit any rows with NA in them using
#' na.omit() functionality.
#'
#' @param orig_dataframe
#' @param created_dataframe
#'
#' @return new merged dataframe built from the 2 input dataframes.
#' @export
#'
#' @examples
merge_created_dataframe_with_original_dataframe<-function(orig_dataframe, created_dataframe){
  returned<-merge(orig_dataframe, created_dataframe)
  returned<-na.omit(returned)
  return(returned)
}

#' filling_na_data_in_xts_object
#'
#' takes in an xts object and attempts to interpolate missing data based on
#' "how" it is instructed to do so. Returns xts object with missing data
#' interpolated.
#'
#' @param xts_obj
#' @param how interpolation method. Options are : na.locf (last observation
#'   carried forward), na.approx (linear interpolation), and na.spline (cubic
#'   spline interplation )
#'
#' @return an xts object with na's interplated the "how" way.
#' @export
#'
#' @examples
filling_na_data_in_xts_object<-function(xts_obj, how="na.locf"){
  if(how=="na.locf"){return(na.locf(xts_obj))}
  if(how=="na.approx"){return(na.approx(xts_obj))}
  if(how=="na.spline"){return(na.spline(xts_obj))}
}

#' create_balanced_weighted_portfolio
#'
#' Incomplete function, not yet ready to be used.
#' GM - 2/16/2018
#'
#' @param portfolio_returns
#' @param weights_vector
#' @param rebalance_on
#'
#' @return
#' @export
#'
#' @examples
create_balanced_weighted_portfolio<-function(portfolio_returns, weights_vector, rebalance_on="months"){
  w_p <- Return.portfolio(R=portfolio_returns, weights = weights_vector, relanace_on=rebalance_on, verbose=TRUE)
  returns <- w_p$returns
  contributions <- w_p$contributions
  start_period_weights <- w_p$BOP.Weight
  end_period_weights <- w_p$EOP.Weight
  start_period_weights <- w_p$BOP.Value
  end_period_weights <- w_p$EOP.Value
}

#' create_year_month_return_matrix
#'
#' Incomplete function: not yet ready to be used
#' GM 2/16/2018
#'
#' @param ticker ticker string of the xts object
#'
#' @return
#' @export
#'
#' @examples
create_year_month_return_matrix <- function(ticker){
  # this function will only work with montly data.
  temp<-to.monthly(Ad(get(ticker)), name= c(ticker, ticker, ticker, ticker))
  close_col_nm<-paste(ticker, ".Close", sep='')
  ticker_returns <- Return.calculate(temp[,close_col_nm])
  table.CalendarReturns(ticker_returns)
}

#' create_weighted_portfolio
#'
#' Incomplete function: not yet ready to be used.
#'
#' GM 2/16/2018
#'
#' @param portfolio_returns
#' @param weights_vector
#'
#' @return
#' @export
#'
#' @examples
create_weighted_portfolio<-function(portfolio_returns, weights_vector){
  w_p<-Return.portfolio(R=portfolio_returns, weights = weights_vector, verbose=TRUE)
  par(mfrow = c(2,1), mar = c(2,4,2,2))
  plot.zoo(w_p)
}

#' calc_rolling_mean_of_returns
#'
#' Takes as input an xts time series object (of price or returns, not restricted
#' to returns), and caculates a moving window average, of length "window". This
#' function uses the rollapply functionality from xts/zoo.
#'
#' @param xts_obj
#' @param window length of the window to caculate the mean over.
#'
#' @return an xts object containing the moving average of time series in xts_obj
#' @export
#'
#' @examples
calc_rolling_mean_of_returns <- function(xts_obj, window=21){
  # default window size is 252/12, 1 month worth of trading data
  return_xts<-rollapply(xts_obj, window, FUN=mean)
  return(return_xts)
}

#' calc_rolling_geom_mean_of_returns
#'
#' Same as calc_rolling_mean_of_returns but returns the geometric mean instead of arithmetic mean.
#'
#' @param xts_obj
#' @param window moving window size/length
#'
#' @return xts_obj with only the newly calculated rolling geometric mean.
#' @export
#'
#' @examples
calc_rolling_geom_mean_of_returns<- function(xts_obj, window=21){
  # default window size is 252/12, 1 month worth of trading data
  return_xts<-rollapply(xts_obj, window, FUN=mean.geometric)
  return(return_xts)
}

#' calc_rolling_std_of_returns
#'
#' calculates a moving window of standard deviation, of window size "window".
#' Returns a new xts object with only this column in it.
#'
#' @param xts_obj xts object containing returns (daily, weekly, monthly, etc...)
#' @param window moving window size/length
#'
#' @return an xts object with the moving standard deviation of price data in
#'   input xts_obj. Column names in the returne object are not transformed, they
#'   are original.
#' @export
#'
#' @examples
calc_rolling_sd_of_returns <- function(xts_obj, window=21){
  # default window size is 252/12, 1 month worth of trading data
  return_xts<- rollapply(xts_obj, window, FUN=sd)
  return(return_xts)
}

#' calc_rolling_sharpe_of_returns
#'
#' @param xts_obj xts input object of raw price data, a result of a call to getSymbols(), most simply
#' @param window default 21 (1 month of trading days)
#' @param mean_type default "geometric", optionally "arithmetic"
#' @param scale the number of period in 1 year; depends on the periodicity of the data in xts_obj
#'
#' @return xts with rolling sharpe of returns
#' #' @export
#'
#' @examples
calc_rolling_sharpe_of_returns <- function(xts_obj, window=21, mean_type="geometric", scale = 252){
  adjusted_close_price_vector<-Ad(xts_obj)
  returns_vector<-ROC(adjusted_close_price_vector)
  sd_xts<-calc_rolling_sd_of_returns(returns_vector, window = 21)
  if (mean_type == "arithmetic"){ mean_xts<-calc_rolling_mean_of_returns(returns_vector, window = 21) }
  else if (mean_type == "geometric"){ mean_xts<-calc_rolling_geom_mean_of_returns(returns_vector, window = 21) }
  return_xts<-mean_xts/sd_xts * sqrt(scale)
  return(return_xts)
}

#' cum_MinMax_AllPrices
#'
#' Takes an xts object of multiple coumns (AD, CL, HI, Lo, OP), or of just one
#' column/price type, and returns a merged xts object with the cummulative max/min of each
#' column for the defined period. Uses the built-in functions in xts of cummin
#' and cummax. Splits the original xts object into period_types of num_periods
#' (e.g. 1 week = num_periods 1, period_type="weeks")
#'
#' @param xts_obj
#' @param period_type "weeks", "months", "days"
#' @param num_periods the number of periods to calculate min and max over.
#'
#' @return a new xts object, with the cummulativce minimum and cummulative
#'   maximum for the period specified by parameters. The returned xts contains a
#'   data point for each datapoint in the original input xts. For example, if
#'   num periods was 4 and period_type was "weeks", then each point in the
#'   original time series will have a new point that is the minimum and maximum
#'   of price so far, with the column headers being
#'   ticker.price_type."cummax"/"cummin"
#' @export
#'
#' @examples
cum_MinMax_AllPrices<-function(xts_obj, period_type="weeks", num_periods=1){
  list_xts_objects<-split(xts_obj, f=period_type,k=num_periods)
  list_of_cummax_xts<-lapply(list_xts_objects, FUN = cummax )
  merged_cummax_xts<-do.call(rbind, list_of_cummax_xts)
  res<-sapply(colnames(merged_cummax_xts), FUN=function(x) paste(x, ".cummax", sep=''))
  names(merged_cummax_xts) <- res
  list_of_cummin_xts<-lapply(list_xts_objects, FUN = cummin )
  merged_cummin_xts<-do.call(rbind, list_of_cummin_xts)
  res<-sapply(colnames(merged_cummin_xts), FUN=function(x) paste(x, ".cummin", sep=''))
  names(merged_cummin_xts)<-res
  merged_xts<-merge(merged_cummin_xts, merged_cummax_xts)
  return(merged_xts)
}

#' get_close_column
#'
#' Wrapper for the xts built-in fucntion: Cl()
#'
#' @param stock_xts_obj the xts object of timeseries data
#'
#' @return returns the close column only of inputted time series data.
#' @export
#'
#' @examples
get_close_column <- function(stock_xts_obj){
  close_column <- Cl(stock_xts_obj)
  return(close_column)
}

#' get_open_column
#'
#' Wrapper for the xts built-in function: Op()
#'
#' @param stock_xts_obj the xts object of timeseries data
#'
#' @return the open column only of the inputted time series data.
#' @export
#'
#' @examples
get_open_column <- function(stock_xts_obj){
  open_column <- Op(stock_xts_obj)
  return(open_column)
}

#' get_high_column
#'
#' Wrapper for the xts built-in function: Hi()
#'
#' @param stock_xts_obj the xts object of timeseries data
#'
#' @return the high column only of the inputted time series data.
#' @export
#'
#' @examples
get_high_column <- function(stock_xts_obj){
  high_column <- Hi(stock_xts_obj)
  return(high_column)
}

#' get_low_column
#'
#' Wrapper for the xts built-in function: Lo()
#'
#' @param stock_xts_obj the xts object of timeseries data
#'
#' @return the low column only of the inputted timeseries data
#' @export
#'
#' @examples
get_low_column <- function(stock_xts_obj){
  low_column <- Lo(stock_xts_obj)
  return(low_column)
}

#' get_adjusted_close_column
#'
#' Wrapper for the xts built-in function: Ad()
#'
#' @param stock_xts_obj
#'
#' @return the adjusted close column only of the inputted timeseries data
#' @export
#'
#' @examples
get_adjusted_close_column <- function(stock_xts_obj){
  adj_column <- Ad(stock_xts_obj)
  return(adj_column)
}

#' get_volumn_column
#'
#' Wrapper for the xts built-ion function: Vo()
#'
#' @param stock_xts_obj
#'
#' @return the volumn column only of the inputted timeseries data
#' @export
#'
#' @examples
get_volume_column <- function(stock_xts_obj){
  vol_column <- Vo(stock_xts_obj)
  return(vol_column)
}

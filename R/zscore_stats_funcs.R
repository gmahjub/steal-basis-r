#' sd_stats_return_helper
#'
#' Calculates the mean and standard deviation of x.
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
sd_stats_returns_helper<-function(x, na.rm = TRUE){
  m<-mean(x, na.rm = na.rm)
  s<-sd(x, na.rm = na.rm)
}
######## Appending to Existing Tibble - Mean, Standard Deviation, 95% CI - of Price(adjusted), DailyLogReturns(DailyLog.return)

#' zscore_stats_helper
#'
#' Calculate the mean, standard deviation, 95% ci upper/lower bound, and
#' annualized sharpe ratio
#'
#' @param x
#' @param headers a list of headers for the returned liist of values (mean, sd,
#'   upper, lower, asr)
#' @param na.rm
#' @param annualize default TRUE, annualize sharpe or not (FALSE)
#' @param scale 252 or daily, periodicity, 52 for weekly, 12 for monthly, 4 for quarterly, etc..
#'
#' @return a dictionary where the values are keyed by the input list, headers
#' @export
#'
#' @examples
zscore_stats_helper<-function(x, headers, na.rm = TRUE, annualize = TRUE, scale = 252){
  m<-mean(x, na.rm = na.rm)
  s<-sd(x, na.rm = na.rm)
  asr <- m/s
  if (annalize){ asr<-asr*sqrt(scale) }
  #95% CI = 2 sd's
  hi<-m + 2*s
  lo<-m - 2*s
  ret <- c(m, s, hi, lo, asr)
  names(ret)<-headers
  return(ret)
}
# here we are calculating the ZScore stats (mean, sd, 95% CI's) for a tibble object
# the object can contain any values you like, either returns or raw prices.
# As demonstrated in next function, selecting DailyLog.return will do the calc on Returns
# make sure to have a column named the "select" column.

#' get_price_ZScore_stats
#'
#' Calculate the Zscore stats (mean, sd, 95% CI's) for a tibble object where the
#' tibble object could be return values or raw price data. This function
#' calculates a rolling window_size Zscore dataset using the helper function
#' zscore_stats_helper(). It uses adjusted closing prices for this calculation.
#'
#' @param tibble_obj most simply, the result of a call to tq_index(), requring a
#'   column named "adjusted" to function properly.
#'
#' @return the original tibble, plus appended columns for mean, asd, hi.95ci,
#'   lo.95ci, asr
#' @export
#'
#' @examples
get_price_ZScore_stats<-function(tibble_obj, periodicity="daily", window_size = 5, annualize = FALSE, scale = 252){
  # this function returns the original tibble in addition to the columns of mean, stdev, hi.95, lo.95
  if (periodicity == "daily"){pl<-"D"}
  else if (periodicity == "weekly"){pl<-"W"}
  else if (periodicity == "monthly"){pl<-"M"}
  else if (periodicity == "quarterly"){pl<-"Q"}
  else if (periodicity == "yearly"){pl<-"Y"}
  header_prefix <- paste(window_size, pl, "AdjP", sep = "")
  col_names<-c(paste(header_prefix, ".mean", sep = ""), paste(header_prefix, ".sd", sep = ""),
             paste(header_prefix, ".hi.95", sep=""), paste(header_prefix, ".lo.95",sep = "" ),
             paste(header_prefix, ".asr", sep = ""))
  sd_stats<-tibble_obj %>% group_by(symbol) %>% tq_mutate(select = adjusted,
                                                          mutate_fun = rollapply,
                                                          width=window_size,
                                                          align="right",
                                                          by.column=FALSE,
                                                          FUN=zscore_stats_helper,
                                                          headers=col_names,
                                                          na.rm=TRUE,
                                                          annualize = annualize,
                                                          scale = scale)
  return(sd_stats)
}

#' get_append_weekly_log_returns
#'
#' Same functionality as the get_weekly_log_returns() function but returns the
#' original tibble_obj with the weekly log returns calculated as a new column.
#' In addition, it fills in the in-between days with the last weekly log return
#' that was calculated. This can be turned off by setting the
#' "fill_in_between_dates" argument to FALSE
#'
#' @param tibble_obj simply a call to tq_get() will achieve this input
#' @param fill_in_between_dates TRUE default
#'
#' @return a tibble object with the appended weekly log returns.
#' @export
#'
#' @examples
get_append_weekly_log_returns<-function(tibble_obj, fill_in_between_dates=TRUE){
  new_tibble_obj<-tibble_obj %>% group_by(symbol) %>% tq_transmute(select=adjusted,
                                                                   mutate_fun = periodReturn,
                                                                   period="weekly",
                                                                   type="log",
                                                                   col_rename="WeeklyLog.return")
  if (fill_in_between_dates){
    new_tibble_obj<-left_join(tibble_obj, new_tibble_obj)
    new_tibble_obj<-na.locf(new_tibble_obj)
  } else { new_tibble_obj<-right_join(tibble_obj, new_tibble_obj)}
  return(new_tibble_obj)
}

#' get_append_monthly_log_returns
#'
#' Calculates montly log returns and appends them to the inputted tibble and
#' returns this tibble. The returns are returns of adjusted close prices month
#' over month. When fill_in_between_dates is set to TRUE (default), the returned
#' tibble will have the original periodicity with the in between dates set to
#' the last calculated monthly return
#'
#' @param tibble_obj most simply, a tibble from a call to tq_get()
#' @param fill_in_between_dates TRUE (default), FALSE, no filling of monthly
#'   returns for in-between dates.
#'
#' @return tibble with monthly returns, in addtion to the columns from the input tibble.
#' @export
#'
#' @examples
get_append_monthly_log_returns<-function(tibble_obj, fill_in_between_dates=TRUE){
  new_tibble_obj<-tibble_obj %>% group_by(symbol) %>% tq_transmute(select=adjusted,
                                                                   mutate_fun = periodReturn,
                                                                   period="monthly",
                                                                   type="log",
                                                                   col_rename="MonthlyLog.return")
  if (fill_in_between_dates){
    new_tibble_obj<-left_join(tibble_obj, new_tibble_obj)
    new_tibble_obj<-na.locf(new_tibble_obj)
  } else {new_tibble_obj<-right_join(tibble_obj, new_tibble_obj)}
  return (new_tibble_obj)
}

#' get_append_quarterly_log_returns
#'
#' Calculates quarterly log returns of adjusted close prices using periodReturns
#' function of xts. Appends new column of returns to existing tibble and returns
#' said tibble. If fill_in_between_dates is TRUE (default), the in between dates
#' will be filled in with the last calculated quarterly return.
#'
#' @param tibble_obj
#' @param fill_in_between_dates TRUE (default) FALSE (to only return quarterly endpoints)
#'
#' @return tibbler with quarterly returns
#' @export
#'
#' @examples
get_append_quarterly_log_returns<-function(tibble_obj, fill_in_between_dates=TRUE){
  new_tibble_obj<-tibble_obj %>% group_by(symbol) %>% tq_transmute(select=adjusted,
                                                                   mutate_fun = periodReturn,
                                                                   period="quarterly",
                                                                   type="log",
                                                                   col_rename="QuarterlyLog.return")
  if (fill_in_between_dates){
    new_tibble_obj<-left_join(tibble_obj, new_tibble_obj)
    new_tibble_obj<-na.locf(new_tibble_obj)
  } else {new_tibble_obj <- right_join(tibble_obj, new_tibble_obj)}
  return (new_tibble_obj)
}

#' get_append_yearly_log_returns
#'
#' Same as get_append_quarterly_log_returns, except yearly
#'
#' @param tibble_obj
#' @param fill_in_between_dates
#'
#' @return
#' @export
#'
#' @examples
get_append_yearly_log_returns<-function(tibble_obj, fill_in_between_dates=TRUE){
  new_tibble_obj<-tibble_obj %>% group_by(symbol) %>% tq_transmute(select=adjusted,
                                                                   mutate_fun = periodReturn,
                                                                   period="yearly",
                                                                   type="log",
                                                                   col_rename="YearlyLog.return")
  if (fill_in_between_dates){
    new_tibble_obj<-left_join(tibble_obj, new_tibble_obj)
    new_tibble_obj<-na.locf(new_tibble_obj)
  } else {new_tibble_obj <- right_join(tibble_obj, new_tibble_obj)}
  return (new_tibble_obj)
}

#' get_returns_ZScore_stats
#'
#' Get the mean, standard dev, 95% confidence intervals, and annualized sharpe
#' ratio of log returns. The input tibble_obj must have a column headered
#' "<periodicity>Log.return". So if the periodicity of the data is daily, then
#' there must be a column in the input tibble labeled, "DailyLog.return".
#'
#'
#' @param tibble_obj input tibble, must at least contain a column <periodicity>Log.return
#' @param periodicity
#' @param window_size
#'
#' @return input tibble object, plus columns appended for the zscore stats
#'   (mean, sd, hi95ci, lo95ci, asr)
#' @export
#'
#' @examples
get_returns_ZScore_stats<-function(tibble_obj, periodicity = "daily", window_size = 5, annualize = TRUE, scale = 252){
  select_what<-"DailyLog.return" # default
  if (periodicity == "daily"){pl<-"D" ; select_what = "DailyLog.return"}
  else if (periodicity == "weekly"){pl<-"W"; select_what = "WeeklyLog.return"}
  else if (periodicity == "monthly"){pl<-"M"; select_what = "MonthlyLog.return"}
  else if (periodicity == "quarterly"){pl<-"Q"; select_what = "QuarterlyLog.return"}
  else if (periodicity == "yearly"){pl<-"Y"; select_what = "YearlyLog.return"}
  header_prefix <- paste(window_size, pl, ".Rets.", sep = "")
  columns_names <- c(paste(header_prefix, "mean", sep = ""), paste(header_prefix, "sd", sep = ""), paste(header_prefix, "hi.95", sep =""), paste(header_prefix, "lo.95", sep=""), paste(header_prefix, "asr", sep = ""))
  sd_stats_of_returns<-tibble_obj %<% group_by(symbol) %<% tq_mutate_(select = select_what,
                                                                      mutate_fun = rollapply,
                                                                      width = window_size,
                                                                      align="right",
                                                                      by.column=FALSE,
                                                                      FUN=zscore_stats_helper,
                                                                      headers=columns_names,
                                                                      na.rm = TRUE,
                                                                      annualize = annualize,
                                                                      scale = scale)
  return(sd_stats_of_returns)
}

#' get_returns_helper_function
#'
#' x is a vector of prices, the function calculates returns of the series x.
#'
#' @param x
#' @param na.rm
#'
#' @return a vector of returns.
#' @export
#'
#' @examples
get_returns_helper_function<-function(x, na.rm = TRUE){
  # uncomment the below if x is a tibble object with mutliple columns
  # right now the assumption is x is a specific column, usually "adjusted"
  #return_rets<-(x[["adjusted"]] / lag(x[["adjusted"]], k = -1) ) - 1
  return_rets<- (x / lag(x, k = -1)) - 1
  return(return_rets)
}

#' close_less_open_div_range
#'
#' Returns a syntheic "day sharpe" for lack of a better term. It is a helper
#' method used by get_close_less_open_div_range. Take the day's close price,
#' minus the day's open price, and divides it by the range (high - low).
#'
#' @param v
#'
#' @return a "day sharpe" value based on open to close return and day's range.
#' @export
#'
#' @examples
close_less_open_div_range<-function(v){
  day_sharpe<-(v$close-v$open)/(v$high-v$low)
  return(day_sharpe)
}
# essentially Day.Sharpe
# small # means price mean reversion
# big # means trend
# negative # means negative return
# positive # means positive return

#' get_close_less_open_div_range
#'
#' Return a tibble object, with a new column, "Day.Sharpe". This value is
#' returned by the helper method close_less_open_div_range(), which is called
#' inside of a call to tq_mutate().
#'
#' @param tibble_obj
#'
#' @return tibble with input tibble columns plus a new column denoted "Day.Sharpe".
#' @export
#'
#' @examples
get_close_less_open_div_range<-function(tibble_obj){
  return_tibble_obj <- tibble_obj %>% group_by(symbol) %>% tq_mutate(select = open:close,
                                                                     mutate_fun=apply.daily,
                                                                     FUN = close_less_open_div_range,
                                                                     col_rename="Day.Sharpe")
  return(return_tibble_obj)
}

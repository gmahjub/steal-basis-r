############# Creating new Tibble/Appending Existing Tibble - Log Returns #########

#' get_daily_log_returns
#'
#' Takes a input a tibble oject (returned from a call to tq_get()) and returns a
#' tibble object containing the daily returns, grouped by symbol (ticker). Daily
#' returns are calculated using the adjusted closing prices, and are geometric
#' returns (type = log of the periodReturn mutate function). This function
#' returns a new tibble object with the resulting columns (Daily Returns). It
#' utilizes tq_transmute vs. tq_mutate.
#'
#' @param tibble_obj
#'
#' @return tibble oject containing the daily returns, grouped by symbol.
#'   Resulting colun name is DailyLog.return.
#' @export
#'
#' @examples
get_idx_component_daily_log_returns<-function(tibble_obj, period = "daily"){
  daily_returns <- tibble_obj %>% group_by(symbol) %>% tq_transmute(select=adjusted,
                                                                    mutate_fun = periodReturn,
                                                                    period=period,
                                                                    type="log",
                                                                    col_rename = "Log.Returns")
  return(daily_returns)
}

get_daily_log_returns<-function(tibble_obj, period = "daily"){
  names(tibble_obj) <- c("Date", "open", "high", "low", "close", "volume", "adjusted", "adjdiff")
  daily_returns<-tibble_obj %>% tq_transmute(select = adjusted,
                                             mutate_fun = periodReturn,
                                             period=period,
                                             type = "log",
                                             col_rename = "Log.Returns")
  return (daily_returns)
}

#' get_period_high_to_next_period_low_return
#'
#' Calculates the return between the high at time t, to the low at time t+1.
#' Depending on what the periodicity is, time is in days, weeks, months, years.
#' Takes as input a tibble object with the min and the max of each price type
#' (op, adcl, hi, lo).  The min_max_ohlc_tibble is returned from the function
#' get_append_hi_to_lo_period_returns(). The periodicity arguement here should
#' match the periodicty of the apply in get_append_hi_to_lo_period_returns().
#'
#' @param periodicity options are: daily, weekly, monthly, yearly
#' @param period_OHLCVA_tibble a tibble of the speicified periodicity. Most
#'   simply, this should be a result of a call to get_period_OHLCVA()
#'
#' @return a tibble object containing period high to next period low returns.
#' @export
#'
#' @examples
get_period_high_to_next_period_low_return<-function(period_OHLCVA_tibble, periodicity="daily"){
  lagged_max_hi<-period_OHLCVA_tibble %>% group_by(symbol) %>% tq_mutate(select = high, mutate_fun = lag.xts, k = 1, col_rename=c("high.lag"))
  periodicity_func<-paste("apply", periodicity, sep=".")
  lagged_max_hi %>% group_by(symbol) %>% tq_transmute_(mutate_fun = periodicity_func,
                                                       FUN = function(x) x$low/x$high.lag - 1, col_rename = "hi.nxtLo.Ret" )
}

#' get_period_close_to_next_period_open_return
#'
#' Calculates the returns from the close @ period p to the open @ period p+1.
#' Essentially, calculates O/N returns with the periodicity is daily
#'
#' @param periodicity options are : daily, weekly, monthly, quarterly, yearly
#' @param period_OHLCVA_tibble a tibble with the period OHLCVA. Most simply, get
#'   this tibble with a call to get_period_OHLCVA().
#'
#' @return a new tibble object with a column named cl.nxtOp.Ret contained the
#'   close to next open period returns
#' @export
#'
#' @examples
get_period_close_to_next_period_open_return<-function(period_OHLCVA_tibble, periodicity="daily"){
  lagged_close<-periodicity_tibble_obj %>% group_by(symbol) %>% tq_mutate(select = adjusted, mutate_fun = lag.xts, k = 1,
                                                                          col_rename=c("adjcl.lag"))
  periodicity_func <- paste("apply", periodicity, sep='.')
  period_close_to_next_period_open_returns_tibble<-lagged_close %>% group_by(symbol) %>%
    tq_transmute_(mutate_fun = periodicity_func, FUN = function(x) x$open/x$adjcl.lag -1, col_rename = "adjCl.nxtOp.Ret")
  return (period_close_to_next_period_open_returns_tibble)
}

#' get_period_low_to_next_period_high_return
#'
#' Calculate the returns between the low @ period p to the high @ period p+1.
#' The input parameter min_max_ohlc_tibble is simply a tibble object returned
#' from get_append_hi_to_lo_period_returns(). The periodicity inputted is daily,
#' weekly, monthly, yearly and shoud match that passed to
#' get_append_hi_to_lo_period_returns(). The reason why we are using the
#' min_max_ohlc_tibble here as opposed to the original price tibble is because
#' changing periodicity of a timeseries does not give us the period's high/low,
#' it gives us the high/low of the endpoint of the period.
#'
#' @param periodicity options are: daily, weekly, monthly, quarterly, yearly.
#' @param period_OHLCVA_tibble a tibble containing the OHLCVA for the specified
#'   periodicity, most simply, get this from a call to get_period_OHLCA()
#'
#' @return a tibble object containing period p low to period p+1 high.
#' @export
#'
#' @examples
get_period_low_to_next_period_high_return<-function(period_OHLCVA_tibble, periodicity="daily"){
  lagged_min_lo<-period_OHLCVA_tibble %>% group_by(symbol) %>% tq_mutate(select = low, mutate_fun = lag.xts, k = 1,
                                                                         col_rename=c("low.lag"))
  periodicity_func<-paste("apply", periodicity, sep='.')
  period_low_to_next_period_high_returns_tibble<-lagged_min_lo %>% group_by(symbol) %>% tq_transmute_(mutate_fun = periodicity_func, FUN = function(x) x$high/x$low.lag - 1,
                                                                                                      col_rename="lo.nxtHi.Ret")
  return (period_low_to_next_period_high_returns_tibble)
}

#' get_period_open_to_close_return
#'
#' Accepts as input a tibble of the specified periodicity, and then calculates
#' the return from the open at period p to the close at period p. This is a same
#' period return, open to close of the same day/week/month/period.
#'
#' @param periodicity options are: daily, weekly, monthly, quarterly, yearly
#' @param period_OHLCVA_tibble a tibble containing OHLCVA data for specified
#'   period, most simply, can be yielded by a call to get_period_OHLCVA()
#'
#' @return a new tibble object with a column for the open_to_close, by period, return.
#' @export
#'
#' @examples
get_period_open_to_close_return<-function(period_OHLCVA_tibble, periodicity="daily"){
  periodicity_func<-paste("apply", periodicity, sep='.')
  op2cl_return_tibble<-period_OHLCVA_tibble %>% tq_transmute_(mutate_fun = periodicity_func,
                                                              FUN = function(x) x$close/x$open - 1,
                                                              col_rename = "op.cl.Ret")
  return (op2cl_return_tibble)
}

#' get_weekly_log_returns
#'
#' Returns the weekly log returns of adjusted close prices, week over week. Uses
#' the built in xts periodReturn function. More specifically, uses adjusted
#' close prices of the last day of week t and the last day of week t-1, to yield
#' a return.
#'
#' @param tibble_obj tibble object, a result of a call to tq_get()
#'
#' @return a new tibble object with the weekly log returns as a new column.
#' @export
#'
#' @examples
get_weekly_log_returns<-function(tibble_obj){
  weekly_log_returns<-tibble_obj %>% group_by(symbol) %>% tq_transmute(select=adjusted,
                                                                       mutate_fun = periodReturn,
                                                                       period="weekly",
                                                                       type="log",
                                                                       col_rename="WeeklyLog.return")
  return(weekly_log_returns)
}

#' get_monthly_log_returns
#'
#' calculates monthly log returns of adjusteed close prices and returns them in
#' a new tibble.
#'
#' @param tibble_obj
#'
#' @return tibble with monthly returns of adjusted close prices
#' @export
#'
#' @examples
get_monthly_log_returns<-function(tibble_obj){
  tibble_returns_obj <-tibble_obj %>% group_by(symbol) %>% tq_transmute(select = adjusted,
                                                                        mutate_fun = periodReturn,
                                                                        period="monthly",
                                                                        type="log",
                                                                        col_rename="MonthlyLog.return")
  return (tibble_returns_obj)
}

#' get_quarterly_log_returns
#'
#' Calculates the quarterly log returns using xts build in periodReturns
#' functionality. Returns are quarter over quarter, using adjusted close prices.
#'
#' @param tibble_obj most simply, returned from a call to tq_get()
#'
#' @return a tibble with the quarterly log returns
#' @export
#'
#' @examples
get_quarterly_log_returns<-function(tibble_obj){
  tibble_returns_obj <-tibble_obj %>% group_by(symbol) %>% tq_transmute(select = adjusted,
                                                                        mutate_fun = periodReturn,
                                                                        period="quarterly",
                                                                        type="log",
                                                                        col_rename="QuarterlyLog.return")
  return(tilbble_returns_obj)
}

#' get_yearly_log_returns
#'
#' Calculate yearly log returns of adjusted close price.
#'
#' @param tibble_obj
#'
#' @return new tibble with yearly log returns of adjusted close price.
#' @export
#'
#' @examples
get_yearly_log_returns<-function(tibble_obj){
  tibble_returns_obj <- tibble_obj %>% group_by(symbol) %>% tq_transmute(select = adjusted,
                                                                         mutate_fun = periodReturn,
                                                                         period="yearly",
                                                                         type="log",
                                                                         col_rename="YearlyLog.return")
  return(tibble_returns_obj)
}

#' get_append_daily_log_returns
#'
#' Calculates daily returns of price time series. utilizes tq_mutate such that
#' input tibble object is appended to with a new column, DailyLog.return. This
#' resulting tibble is then returned.
#'
#' @param tibble_obj tibble object, most simply a result of a call to tq_get()
#'
#' @return a new tibble object with the DalyLog.return column appended to the
#'   original input tibble
#' @export
#'
#' @examples
get_append_daily_log_returns<-function(tibble_obj){
  # tq_mutate adds columns to the existing tibble
  # tq_transmute does not, it returns the columns in a new tibble
  daily_returns<-tibble_obj %>% group_by(symbol) %>% tq_mutate(select=adjusted,
                                                               mutate_fun=periodReturn,
                                                               period="daily",
                                                               type="log",
                                                               col_rename="DailyLog.return")
  return(daily_returns)
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

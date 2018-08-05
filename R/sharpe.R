#' get_period_open_to_close_sharpe
#' 
#' Sharpe of open to close returns.
#'
#' @param op2cl_returns_tibble
#' @param window_size
#' @param annualize defaults to FALSE, if annualize sharpe desired, set to TRUE
#' @param scale 252 for daily periodicity, 52 for weekly, 12 for monthly.
#'
#' @return tibble object with period open to close sharpe.
#' @export
#'
#' @examples
get_period_open_to_close_sharpe<-function(op2cl_returns_tibble, window_size = 5, annualize = FALSE, scale = 252){
  period_open_to_close_sharpe_tibble<-op2cl_returns_tibble %>% group_by(symbol) %>% if_else(!annualize,
                                                                                            tq_mutate(mutate_fun = rollapply,
                                                                                                      width = window_size,
                                                                                                      FUN = function(x)
                                                                                                        mean(x$op.cl.Ret)/sd(x$op.cl.Ret),
                                                                                                      by.column = FALSE,
                                                                                                      col_rename = "op.cl.Sharpe"),
                                                                                            tq_mutate(mutate_fun = rollapply,
                                                                                                      width = window_size,
                                                                                                      FUN = function(x)
                                                                                                        mean(x$op.cl.Ret)/sd(x$op.cl.Ret)*sqrt(scale),
                                                                                                      by.column = FALSE,
                                                                                                      col_rename = "op.cl.Sharpe"))
  return(period_open_to_close_sharpe_tibble)
}

#' get_rolling_Sharpe_AdCl_to_AdCl
#'
#' Calculates a rolling sharpe ratio of returns calculated using adjusted close
#' prices. The returns are calculated day over day, week over week, month over
#' month, etc..., depending on the value of periodicity. Finally, we use
#' tq_mutate to calcularte a rolling non-annualized sharpe ratio, where the size
#' of the rolling window is specified in the parameter num_periods.
#'
#' @param tibble_obj
#' @param num_periods size of rolling window for calc of sharpe ratio
#' @param periodicity daily, weekly, monthly, quarterly, yearly
#' @param annualize defaults to FALSE, set to TRUE if annualzied sharpe desired
#' @param scale 252 for daily periodicity, 52 for weekly, 12 for monthly, etc...
#'
#' @return a tibble, all columns from tibble obj input and a new column named "Ad2Ad.Sharpe..."
#' @export
#'
#' @examples
get_rolling_Sharpe_AdCl_to_AdCl<-function(tibble_obj, num_periods, periodicity="daily", annualize = FALSE, scale = 252){
  if (periodicity == "daily"){period_returns_tibble_obj<-get_append_daily_log_returns(tibble_obj, fill_in_between_dates = FALSE)}
  if (periodicity == "weekly"){period_returns_tibble_obj<-get_append_weekly_log_returns(tibble_obj, fill_in_between_dates = FALSE)}
  if (periodicity == "monthly"){period_returns_tibble_obj<-get_append_monthly_log_returns(tibble_obj, fill_in_between_dates = FALSE)}
  if (periodicity == "quarterly"){period_returns_tibble_obj<-get_append_quarterly_log_returns(tibble_obj, fill_in_between_dates = FALSE)}
  if (periodicity == "yearly"){period_returns_tibble_obj<-get_append_yearly_log_returns(tibble_obj, fill_in_between_dates = FALSE)}
  num_cols<-length(names(period_returns_tibble_obj))
  rets_col_nm <- names(period_returns_tibble_obj)[num_cols]
  new_col_nm<-paste("Ad2Ad.Sharpe", ".", rets_col_nm, sep='')
  if (annualize){
    sharpe_tibble<-period_returns_tibble_obj %>% group_by(symbol) %>% tq_mutate(select = rets_col_nm,
                                                                                mutate_fun = rollapply,
                                                                                width = num_periods,
                                                                                align="right",
                                                                                by.column=FALSE,
                                                                                FUN = function(x) mean(x)/sd(x)*sqrt(scale),
                                                                                col_rename = c(new_col_nm))
  } else {
    sharpe_tibble<-period_returns_tibble_obj %>% group_by(symbol) %>% tq_mutate(select = rets_col_nm,
                                                                                mutate_fun = rollapply,
                                                                                width = num_periods,
                                                                                alight = "right",
                                                                                by.column = FALSE,
                                                                                FUN = function(x) mean(x)/sd(x),
                                                                                col_rename = c(new_col_nm))
  }
  return(sharpe_tibble)
}

#' get_period_low_to_next_period_high_sharpe
#' 
#' Sharpe of the return betwen period t low to period t+1 high
#'
#' @param period_low_to_next_period_high_returns_tibble
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
get_period_low_to_next_period_high_sharpe<-function(period_low_to_next_period_high_returns_tibble, window_size = 5){
  period_low_to_next_period_high_sharpe_tibble<-period_low_to_next_period_high_returns_tibble %>% group_by(symbol) %>%
    tq_mutate(mutate_fun = rollapply,
              width = window_size,
              FUN = function(x) mean(x$lo.nxtHi.Ret)/sd(x$lo.nxtHi.Ret),
              by.column = FALSE,
              col_rename = "lo.nxtHi.Sharpe")
  return(period_low_to_next_period_high_sharpe_tibble)
}

#' get_period_close_to_next_period_open_sharpe
#' 
#' Sharpe of returns between close @ period t to open at period t+1.
#' Basically the sharpe of overnight returns.
#'
#' @param period_close_to_next_period_open_returns_tibble
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
get_period_close_to_next_period_open_sharpe<-function(period_close_to_next_period_open_returns_tibble, window_size = 5){
  period_close_to_next_period_open_sharpe_tibble<-period_close_to_next_period_open_returns_tibble %>% group_by(symbol) %>%
    tq_mutate(mutate_fun = rollapply,
              width = window_size,
              FUN = function(x) mean(x$adjCl.nxtOp.Ret)/sd(x$adjCl.nxtOp.Ret),
              by.column = FALSE,
              col_rename = "adjCl.nxtOp.Sharpe")
  return(period_close_to_next_period_open_sharpe_tibble)
}

#' get_period_high_to_next_period_low_sharpe
#' 
#' Sharpe of returns between high at period t to low at period t+1.
#' Rolling sharpe of length window size.
#'
#' @param period_high_to_next_period_low_returns_tibble
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
get_period_high_to_next_period_low_rolling_sharpe<-function(period_high_to_next_period_low_returns_tibble, window_size=5){
  period_high_to_next_period_low_returns_tibble %>% group_by(symbol) %>% tq_mutate(mutate_fun = rollapply,
                                                                                   width = window_size,
                                                                                   FUN = function(x) mean(x$hi.nxtLo.Ret)/sd(x$hi.nxtLo.Ret),
                                                                                   by.column = FALSE,
                                                                                   col_rename = "hi.nxtLo.Sharpe")
  return(period_high_to_next_low_sharpe_tibble)
}

#' get_period_high_to_next_period_low_sharpe
#' 
#' Sharpe of returns between high @ period t to low @ period t+1.
#' Rolling sharpe of length window size.
#'
#' @param period_high_to_next_period_low_returns_tibble
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
get_period_high_to_next_period_low_rolling_sharpe<-function(period_high_to_next_period_low_returns_tibble, window_size=5){
  period_high_to_next_period_low_returns_tibble %>% group_by(symbol) %>% tq_mutate(mutate_fun = rollapply,
                                                                                   width = window_size,
                                                                                   FUN = function(x) mean(x$hi.nxtLo.Ret)/sd(x$hi.nxtLo.Ret),
                                                                                   by.column = FALSE,
                                                                                   col_rename = "hi.nxtLo.Sharpe")
  return(period_high_to_next_low_sharpe_tibble)
}

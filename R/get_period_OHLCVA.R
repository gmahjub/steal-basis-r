#' get_period_OHLCVA
#'
#' Return the open, high, low, close, cum volume, and adjusted close of the
#' period. This takes the original daily data and returns the first open in the
#' period, the last close/adj of the period and the max and the min respectively
#' of the high and low columns for the period.
#'
#' @param tibble_obj a result of a call to tq_get(), daily data, most simply
#' @param periodicity options are: daily, weekly, monthly, quarterly, yearly
#'
#' @return a new tibble object with OHLCVA rows for the specified periodicity.
#' @export
#'
#' @examples
get_period_OHLCVA<-function(tibble_obj, periodicity="daily"){
  periodicity_func <- paste("apply", periodicity, sep='.')
  periodicity_func <- c(periodicity_func)
  opening_open_of_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select ="open",
                                                                              mutate_fun = periodicity_func,
                                                                              FUN = function(x) x[1],
                                                                              col_rename = "open")
  max_high_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "high",
                                                                          mutate_fun = periodicity_func,
                                                                          FUN = max,
                                                                          col_rename = "high")
  min_low_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "low",
                                                                         mutate_fun = periodicity_func,
                                                                         FUN = min,
                                                                         col_rename = "low")
  cum_volume_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "volume",
                                                                            mutate_fun = periodicity_func,
                                                                            FUN = sum,
                                                                            col_rename = "volume")
  closing_unadj_close_of_period <- tibble_obj %>% group_by(sbymol) %>% tq_transmute_(select = "close",
                                                                                     mutate_fun = periodicity_func,
                                                                                     FUN = function(x) x[length(x)],
                                                                                     col_rename = "close")
  closing_adj_of_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "adjusted",
                                                                             mutate_fun = periodicity_func,
                                                                             FUN = function(x) x[length(x)],
                                                                             col_rename = "adjusted")
  return_tibble <- left_join(opening_open_of_period, max_high_in_period)
  return_tibble <- left_join(return_tibble, min_low_in_period)
  return_tibble <- left_join(return_tibble, closing_unadj_close_of_period)
  return_tibble <- left_join(return_tibble, cum_volume_in_period)
  return_tibble <- left_join(return_tibble, closing_adj_of_period)
  return (return_tibble)
}

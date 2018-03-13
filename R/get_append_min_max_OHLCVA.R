#' get_append_min_max_OHLCV
#'
#' This function will return the min and max of each of the columns in the
#' inputted tibble for the specified period.
#'
#'
#' @param tibble_obj
#' @param periodicity options are: daily, weekly, monthly, quarterly, yearly
#'
#' @return at tibble with the min and max of each column in the input tibble object
#' @export
#'
#' @examples
get_append_min_max_OHLCVA<-function(tibble_obj, periodicity="daily"){
  require(tidyquant)
  require(tidyverse)
  periodicity_func <- paste("apply", periodicity, sep='.')
  periodicity_func <- c(periodicity_func)
  max_high_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "high",
                                                                          mutate_fun = periodicity_func,
                                                                          FUN = max,
                                                                          col_rename = "max.high")
  min_low_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "low",
                                                                         mutate_fun = periodicity_func,
                                                                         FUN = min,
                                                                         col_rename = "min.low")
  max_open_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "open",
                                                                          mutate_fun = periodicity_func,
                                                                          FUN = max,
                                                                          col_rename = "max.open")
  max_close_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "adjusted",
                                                                           mutate_fun = periodicity_func,
                                                                           FUN = max,
                                                                           col_rename = "max.adj")
  max_low_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "low",
                                                                         mutate_fun = periodicity_func,
                                                                         FUN = max,
                                                                         col_rename = "max.low")
  max_unadj_close_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "close",
                                                                                 mutate_fun = periodicity_func,
                                                                                 FUN = max,
                                                                                 col_rename = "max.close")
  min_volume_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "volume",
                                                                            mutate_fun = periodicity_func,
                                                                            FUN = min,
                                                                            col_rename = "min.volume")
  max_volume_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "volume",
                                                                            mutate_fun = periodicity_func,
                                                                            FUN = max,
                                                                            col_rename = "max.volume")
  min_max_ohlc_tibble <- left_join(max_open_in_period, max_high_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, max_low_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, max_unadj_close_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, max_volume_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, max_close_in_period)
  min_open_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "open",
                                                                          mutate_fun = periodicity_func,
                                                                          FUN = min,
                                                                          col_rename = "min.open")
  min_close_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "adjusted",
                                                                           mutate_fun = periodicity_func,
                                                                           FUN = min,
                                                                           col_rename = "min.adj")
  min_high_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "high",
                                                                          mutate_fun = periodicity_func,
                                                                          FUN = min,
                                                                          col_rename = "min.high")
  min_unadj_close_in_period <- tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = "close",
                                                                                 mutate_fun = periodicity_func,
                                                                                 FUN = min,
                                                                                 col_rename = "min.close")
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, min_open_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, min_high_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, min_low_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, min_unadj_close_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, min_volume_in_period)
  min_max_ohlc_tibble <- left_join(min_max_ohlc_tibble, min_close_in_period)
  return(min_max_ohlc_tibble)
}

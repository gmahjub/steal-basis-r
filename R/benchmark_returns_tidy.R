#' get_benchmark_period_returns_tidy
#'
#' Excactly the same functionality as get_benchmark_period_returns() but instead
#' of using xts functionality and returning XTS data structure, it uses the
#' tidyquant functionality and tibble data structure.
#'
#' @param etf_to_sector_mapping_tibble
#' @param start_date
#' @param price_type options are: open, high, low, close, adjusted
#' @param periodicity options are: daily, weekly, monthly, quarterly, yearly
#'
#' @return benchmark returns in tibble obj
#' @export
#'
#' @examples
get_benchmark_period_returns_tidy<-function(etf_to_sector_mapping_tibble, start_date="2017-11-01", periodicity="daily", price_type="adjusted"){
  prices <- etf_to_sector_mapping_tibble %>% tq_get(get="stock.prices", from=start_date) %>% group_by(ticker, sector)
  #etf_period_returns <- prices %>% tq_transmute(select=adjusted, mutate_fun = periodReturn, period=period_size, type='log')
  etf_period_returns <- prices %>% tq_transmute(select = price_type, mutate_fun=periodReturn, period=period_size, type='log')
  return(etf_period_returns)
}


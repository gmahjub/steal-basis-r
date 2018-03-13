#' sector_index_corr_calc_tidy
#'
#' Exact same as sector_index_corr_calc but using the tidyquant library and
#' utilizing tibble objects
#'
#' @param tidy_etf_returns
#' @param window
#'
#' @return tibble object of sector returns and corresponding correlations
#'   between sector returns and overall market index returns
#' @export
#'
#' @examples
sector_index_corr_calc_tidy<-function(tidy_etf_returns, window){
  require(rlang)
  period_of_return<-names(select(ungroup(tidy_etf_returns), ends_with("returns")))
  old_index_col_nm<-get(period_of_return)
  index_etf_ticker<-distinct(tidy_etf_returns, ticker) %>% ungroup() %>% filter(sector=="ZZZ.Index") %>% select(ticker)
  new_index_col_nm<-paste(index_etf_ticker$ticker, "daily.returns", sep=".")
  iso_index_tibble <- tidy_etf_returns %>% ungroup() %>% filter(ticker == index_etf_ticker$ticker) %>% select(date, ends_with("returns"))
  iso_index_tibble <- iso_index_tibble %>% dplyr::rename(!!new_index_col_nm := !!old_index_col_nm)
  etfs_mkt_rets_joined<-inner_join(tidy_etf_returns, iso_index_tibble, by="date")
  corrs<-tq_mutate_xy_(etfs_mkt_rets_joined, x = old_index_col_nm, y = new_index_col_nm, mutate_fun = c("runCor"), n=window, col_rename = "Correl")
  return(corrs)
}

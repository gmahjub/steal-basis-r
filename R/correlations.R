#' sector_index_corr_calc_xts
#'
#' Calculates the sector versus index correlation, such as SPY versus a sector
#' specific ETF (XLF) or a single stock. This function is part of a workflow. In
#' order to get the input parameter benchmark_etf_daily_returns, we need to call
#' get_benchmark_period_returns(). We then call this function to get the
#' correlation of the sector etf to the entire index.
#'
#' @param benchmark_etf_daily_returns xts with period returns for benchmark
#'   sector etf's and overall market etf.
#' @param window size of window for rolling correlation calculation in number of
#'   periods (e.g 10 days, 2 weeks)
#'
#' @return xts object with columns for sector etf returns, index etf returns,
#'   and corresponding correlation between each sector and the overall index.
#' @export
#'
#' @examples
sector_index_corr_calc_xts<-function(benchmark_etf_daily_returns, window){
  # create one XTS object to hold both price series
  list_of_etfs<-colnames(benchmark_etf_daily_returns)
  new_returnable_xts<-merge(benchmark_etf_daily_returns[,1], benchmark_etf_daily_returns[, length(list_of_etfs)])
  new_returnable_xts$rolling_cor<-rollapply(new_returnable_xts, window, function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"), by.column=FALSE)
  first_sector_etf<-list_of_etfs[1]
  total_index_etf<-list_of_etfs[length(list_of_etfs)]
  correl_col_nm<-paste(first_sector_etf, window, "Per.Roll.Corr", total_index_etf, sep=".")
  names(new_returnable_xts)<-c(paste(first_sector_etf, "Rets", sep="."), paste(total_index_etf, "Rets", sep="."), correl_col_nm)
  for (i in 2:length(list_of_etfs) ){
    this_etf<-list_of_etfs[i]
    merged_xts<-merge(benchmark_etf_daily_returns[,i], benchmark_etf_daily_returns[,length(list_of_etfs)])
    saved_col_names<-names(new_returnable_xts)
    new_returnable_xts$rolling_cor<-rollapply(merged_xts, window, function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"), by.column=FALSE)
    new_returnable_xts$sec_ret<-benchmark_etf_daily_returns[,i]
    new_col_nm<-paste(this_etf, window, "Per.Roll.Corr", total_index_etf, sep=".")
    names(new_returnable_xts)<-c(saved_col_names, new_col_nm, paste(this_etf, "Rets", sep="."))
  }
  return(new_returnable_xts)
}

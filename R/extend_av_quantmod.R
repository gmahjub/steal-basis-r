#' av_Main_retrieve_and_write
#'
#' Alphavantage main retrieve and write function, retrieve data and write out to
#' its own directory.
#'
#' @param tickers list of tickers
#' @param remote_src "av" for alphavantage
#' @param start_date start date of tick pull, doesn't get considered with
#'   alphavantage, doing a full tick pull everytime right now, output.size =
#'   "full".
#'
#' @return list of tickers
#' @export
#'
#' @examples
av_Main_retrieve_and_write<-function(tickers, path_to_ticker_dir, remote_src="av", start_date="2017-11-01", end_date = Sys.Date()){
  #using getSymbols() here and using xts only, not tidy yet
  list_of_changed_tickers<-sapply(tickers, FUN=check_Rcompat_ticker_names_av, path_to_ticker_dir, start_date=start_date, remote_src=remote_src)
  list_of_changed_tickers<-list_of_changed_tickers[!is.na(list_of_changed_tickers)]
  tickers<-setdiff(tickers, list_of_changed_tickers)
  for (ticker in tickers){do.call("<<-", list(ticker, get(getSymbols(ticker, src=remote_src, api.key=get_alphavantage_api_key(), adjusted=TRUE, output.size="full"))))}
  adjustOHLC_wrapper(tickers)
  lapply(tickers, FUN=write_ticker_csv, path_to_ticker_dir = path_to_ticker_dir)
  return(tickers)
}

#' check_Rcompat_ticker_names_av
#'
#' Check for R compatibiity of ticker names. Anything with a '-' or tickers in
#' list (C, T, F, NA) are changed as these are reserved characters/strings in R.
#' Also, calls getSymbols and write_ticker_csv for those tickers that are
#' changed. Finally, returns a list of those tickers that were changed.
#'
#' @param path_to_ticker_dir ticker directory for writing out alphavantage ticks
#' @param start_date
#' @param remote_src "av" for alphavantage
#'
#' @return a list of tickers that were changed.
#' @export
#'
#' @examples
check_Rcompat_ticker_names_av<-function(path_to_ticker_dir, start_date, remote_src, end_date = Sys.Date()){
  setDefaults(getSymbols.av, api.key = get_alphavantage_api_key())
  changed_ticker<-NA
  require(stringr)
  tf<-str_detect(string=ticker, pattern=fixed("-"))
  if (tf) {
    m <- str_replace(string=ticker, pattern=fixed("-"), replacement=".")
    do.call("<<-", list(m, get(getSymbols(ticker, src=remote_src, api.key=get_alphavantage_api_key(), adjusted=TRUE, output.size="full"))))
    col_names<-colnames(get(m))
    vec_new_col_nm<-c()
    for(name in col_names){
      new_col_name <-str_replace(string=name, pattern=fixed("-"), replacement=".")
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    changed_ticker<-ticker
    adjustOHLC_wrapper(m)
    write_ticker_csv(m, path_to_ticker_dir, column_names=vec_new_col_nm)
  }
  ticker_confused_with_bool <- c('C', 'T', 'F', 'NA')
  if(ticker %in% ticker_confused_with_bool){
    m <- paste(ticker, ".", ticker, sep='')
    do.call("<<-", list(m, get(getSymbols(ticker, src=remote_src, api.key=get_alphavantage_api_key(), adjusted=TRUE, output.size="full"))))
    col_names<-colnames(get(m))
    vec_new_col_nm<-c()
    for (name in col_names){
      new_col_name <- paste(ticker, ".", name, sep='')
      vec_new_col_nm<-c(vec_new_col_nm, new_col_name)
    }
    changed_ticker<-ticker
    adjustOHLC_wrapper(m)
    write_ticker_csv(m, path_to_ticker_dir, column_names = vec_new_col_nm)
  }
  return(changed_ticker)
}

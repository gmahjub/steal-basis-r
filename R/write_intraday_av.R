#' write_intraday_av
#'
#' Write to csv file an intraday time series object retrieved from alphavantage.
#'
#' @param ticker
#' @param path_to_ticker_dir
#' @param column_names
#'
#' @return nothing
#' @export
#'
#' @examples
write_intraday_av <- function(ticker, data_obj, path_to_ticker_dir, column_names = NULL){
  ticker_csv_file <- paste(path_to_ticker_dir, ticker, ".csv", sep = "")
  if (!is.null(column_names)){
    data_obj<-data_obj %>% setNames(., column_names)
  } else {
    data_obj<-data_obj %>% setNames(., c(" ", "BarTimeStamp", "Open", "High", "Low", "Close", "Volume"))
  }
  write.csv(data_obj, file = ticker_csv_file)
}

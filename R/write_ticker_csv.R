#' write_ticker_csv
#'
#' writes an xts time series data object to flat file csv format.
#'
#' @param ticker the ticker name, used to create the ticker csv filename, and
#'   also to retrieve time seriers data object.
#' @param path_to_ticker_dir path to the directory where the ticker csv file
#'   will be written.
#' @param column_names column names of the csv file to be written.
#'
#' @return currently returns nothing.
#' @export
#'
#' @examples
write_ticker_csv <- function(ticker, path_to_ticker_dir, xts_obj = NULL, column_names=NULL) {
  path <- path_to_ticker_dir
  ticker_csv_file <- paste(path, ticker, ".csv", sep="")
  if (is.null(xts_obj))
    stuff_to_write <<- get(ticker, envir=parent.frame())
  else
    stuff_to_write <<- xts_obj
  if (!is.null(column_names)){
    colnames(stuff_to_write)<<-make.names(names=column_names)
  }
  write.zoo(as.xts(stuff_to_write), ticker_csv_file, index.name="Date",sep=",")
}

#' write_intraday_IBKR
#'
#' Write out IBKR ticks file. They don't have to be intraday ticks, we use this
#' same function for daily bars also.
#'
#' @param ticker
#' @param data_obj
#' @param path_to_ticker_dir
#' @param column_names
#' @param intraday
#'
#' @return
#' @export
#'
#' @examples
write_intraday_IBKR <- function(ticker, data_obj, path_to_ticker_dir, column_names=NULL, intraday = TRUE ){
  message(paste("path_to_ticker_dir is ", path_to_ticker_dir, " and ticker is ", ticker, sep = ""))
  ticker_csv_file <- paste(path_to_ticker_dir, ticker, ".csv", sep = "")
  write.zoo(as.xts(data_obj), ticker_csv_file, index.name="BarTimeStamp", sep = ",")
}
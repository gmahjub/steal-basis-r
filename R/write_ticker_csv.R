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
write_ticker_csv <- function(ticker, path_to_ticker_dir, column_names=NULL) {
  path <- path_to_ticker_dir
  ticker_csv_file <- paste(path, ticker, ".csv", sep="")
  stuff_to_write <<- get(ticker, envir=parent.frame())
  if (!is.null(column_names)){
    colnames(stuff_to_write)<<-make.names(names=column_names)
  }
  write.zoo(as.xts(stuff_to_write), ticker_csv_file, index.name="Date",sep=",")
}

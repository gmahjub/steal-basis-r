#' get_tickers_from_local_holdings_file
#'
#' Accepts as input path to a local CSV file. The default is comma delimited,
#' containing a header, and reading all rows in the file. If there is no header,
#' set header to FALSE, and if only a certain number of rows are to be read,
#' specifiy the number of rows in the nrows parameter. Requires that the column
#' with the tickers must he headered "Ticker".
#'
#' @param file_path path to file to read containing the holdings as string
#'   tickers.
#' @param header TRUE if file contains a header row, FALSE if not. Defaulted to
#'   TRUE
#' @param sep value seperation, defaulted to comma since CSV is expected.
#' @param n_rows number of rows to read (post header if header is TRUE),
#'   defaulted to all rows in the file.
#'
#' @return a vector of chars, where each element is a ticker.
#' @export
#'
#' @examples
get_tickers_from_local_holdings_file<-function(file_path, sep=",", n_rows=-1){
  tickers<-read.csv(file=file_path, header=TRUE, sep=sep, nrows=n_rows)
  tickers<-as.vector(tickers$Ticker)
  return(tickers)
}

#' get_holdings_from_file_in_tibble_fmt
#'
#' get all columns and rows in tibble format for the local holding file input
#'
#' @param file_path path to locaiton of holdings file
#' @param sep default ","
#' @param n_rows default read all rows in file
#'
#' @return a tibble file containing the data in the local holdings csv file.
#' @export
#'
#' @examples
get_holdings_from_file_in_tibble_fmt<-function(file_path, sep=",", n_rows = -1){
  holdings_tibble<-as.tibble(read.csv(file=file_path, header=TRUE, sep=sep, nrows=n_rows))
  return (holdings_tibble)
}

#' get_holdings_filtered
#'
#' filters the inputted tilbble based on the conditions specified in paramters.
#' For example, if filter_column name is "Asset.Class" and filter operator is
#' "==", and the filter condition is "Equity", the we will returned all the rows
#' in the input tibble object that has the Asset.Class column equal to "Equity".
#'
#' @param holdings_tibble_obj tibble object with index holdings, tickers, and possibly more information.
#' @param filter_column_name the column to filter on
#' @param filter_operator the filter operator : "==", ">", "<", etc...
#' @param filter_value the value to filter for.
#'
#' @return a tibbe obj, filtered based on condition.
#' @export
#'
#' @examples
get_holdings_filtered<-function(holdings_tibble_obj, filter_column_name, filter_operator, filter_value){
  col_exists<-which(names(holdings_tibble_obj) == filter_column_name) < 1
  if (length(col_exists) < 1){
    message(paste("column ", filter_column_name, " does not exist in the input tibble object!", sep = "")) 
    return (holdings_tibble_obj)
  }
  assign(eval(filter_column_name), filter_column_name)
  str_filter_cond<-paste(get(filter_column_name), filter_operator, "filter_value", sep=" ")
  returned_tibble <- holdings_tibble_obj %>% group_by(get(filter_column_name)) %>% filter_(str_filter_cond)
  return (returned_tibble)
}

#' load_intraday_timeseries_tibble
#'
#' Load the intraday ticker file into a tibble object
#'
#' @param ticker
#' @param path_to_ticker_dir
#'
#' @return
#' @export
#'
#' @examples
load_intraday_timeseries_tibble<-function(ticker, path_to_ticker_dir){
  total_path_to_csv_file<-paste(path_to_ticker_dir, ticker, ".csv", sep = "")
  message(paste("opening file ", total_path_to_csv_file, sep = ""))
  tibble_intraday_ts<-tryCatch(as.tibble(read.csv(total_path_to_csv_file, header=TRUE, sep = ",", colClasses = c("character", "POSIXct", "numeric", "numeric", "numeric", "numeric", "integer"))),
                               error = function (e) { as.tibble(read.csv(total_path_to_csv_file, header = TRUE, sep = ","))})
  isBarTimeStamp<-colnames(tibble_intraday_ts[,1]) == "BarTimeStamp"
  # the first column should always be the timestamp. The only exception is when we have a counter in the first column.
  if (!isBarTimeStamp){
    tibble_intraday_ts<-tibble_intraday_ts[,2:ncol(tibble_intraday_ts)]
  }
  tibble_intraday_ts<-tibble_intraday_ts %>% mutate(BarTimeStamp = as.POSIXct(BarTimeStamp, format = "%Y-%m-%d %H:%M:%S"))
  return(tibble_intraday_ts)
}



#' load_intraday_timeseries_xts
#'
#' Intrday timeseries file is in csv format. Read the csv and store in an xts
#' timeseries object.
#'
#' @param ticker
#' @param path_to_ticker_dir
#'
#' @return
#' @export
#'
#' @examples
load_intraday_timeseries_xts<-function(ticker, path_to_ticker_dir){
  total_path_to_csv_file<-paste(path_to_ticker_dir, ticker, ".csv", sep = "")
  message(paste("opening file ", total_path_to_csv_file, sep = ""))
  xts_intraday_ts<-read.csv(total_path_to_csv_file, header=TRUE, sep = ",",
                            colClasses = c("character", "POSIXct", "numeric", "numeric", "numeric",
                                           "numeric", "integer"))
  xts_intraday_ts<-xts_intraday_ts[,2:ncol(xts_intraday_ts)]
  return (xts_intraday_ts)
}

#' filter_benchmark_by_sector
#'
#' takes an inputted tibble object and returns a new tibble with the filter
#' applied. In this function, the filter is sector name, thereby we are
#' returning a tibble with only the specified sector name in it. The inputted
#' tibble_obj must have a column titled "sector".
#'
#' @param tibble_obj a tibble, must have a column titled sector for this to work.
#' @param sector_name the sector name to filter for , e.g. Industrials, Financials, etc...
#'
#' @return a new tibble object with only the sector specified in the sector column.
#' @export
#'
#' @examples
filter_benchmark_by_sector<-function(tibble_obj, sector_name){
  op<-"=="
  return_tibble_obj<-filter(tibble_obj, do.call(op, list(sector,sector_name)))
  return(return_tibble_obj)
}

#' create_sector_benchmark_tibble
#'
#' This function takes a inputted tibble object, which results from a call to
#' tq_index(). tq_index takes as a parameter an index name (where valid index
#' names can be found with a call to tq_index_options). The returned tibble from
#' tq_index("DOW") for example must have a sector column. This function is part
#' of a workflow which involves the creation of the inputted tibble object via
#' get_all_stocks_in_index(). This function returns a tibble with each row being
#' a (benchmark etf, sector) pair. For example, (XLY, Consumer
#' Discretinary),(XLE, Energy), (DIA, ZZZ.Index). Notice the index benchmark etf
#' is included here, and to ensure it shows up last in the tibble, the sector is
#' appended with "ZZZ".
#'
#' @param tibble_obj most simply, a result of a call to tq_index()
#' @param index_name options are: tq_index_options() defaulted to "DOW". Should
#'   match the index used when input tibble_obj was created.
#'
#' @return a tibble object with (benchmark etf ticker, sector
#'   description) pairs
#' @export
#'
#' @examples
create_sector_benchmark_tibble<-function(tibble_obj, index_name="DOW"){
  # the tibble here must have a sector column
  distinct_sectors_from_tibble<-distinct(tibble_obj, sector)
  distinct_sectors_from_tibble<-add_row(distinct_sectors_from_tibble, sector="ZZZ.Index")
  sorted_sectors<-arrange(distinct_sectors_from_tibble, sector)
  etf_ticker_sector<-c()
  if (index_name == "DOW"){etf_ticker_sector<-c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLK","XLB", "VOX","DIA")}
  if (index_name == "SP500"){etf_ticker_sector<-c("XLY", "XLP","XLE","XLF","XLV", "XLI", "XLK", "XLB", "XLRE", "VOX", "XLU", "SPY")} # large caps
  if (index_name == "RUSSELL2000"){etf_ticker_sector<-c("XLY", "XLP", "XLE", "XLF", "XLV", "XLB", "XLI", "XLK", "XLU", "IWM")} # small caps
  if (index_name == "SP400"){etf_ticker_sector<-c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLK", "XLB", "XLRE", "VOX", "XLU", "IJH" )}# mid caps
  returned_tibble_obj<-tibble(etf_ticker_sector, sorted_sectors$sector)
  colnames(returned_tibble_obj)<-c("ticker", "sector")
  return(returned_tibble_obj)
}

#' filter_benchmark_by_weight
#'
#' Filter the inputted tibble object by its weighting in the index. The weight
#' column is returned with retrieving a tibble from tq_index(). The op operator
#' can be specified, such that we can look for weight values that greater, less
#' than, or equal to some value, weight_value.
#'
#' @param tibble_obj a tibble object, most simply the result of a call to tq_index(<some_index>)
#' @param weight_value the value of weight to compare the weight column against.
#' @param op options are "==", ">", "<", "<=", etc...
#'
#' @return a tibble with the filtering based on weight applied.
#' @export
#'
#' @examples
filter_benchmark_by_weight<-function(tibble_obj, weight_value, op){
  return_tibble_obj<-filter(tibble_obj, do.call(op, list(weight, weight_value)))
  return(return_tibble_obj)
}

#' filter_benchmark_by_symbol
#'
#' filter the inputted tibble object by symbol. Return only the symbols equal to
#' the symbol specifid by symbol_value
#'
#' @param tibble_obj
#' @param symbol_value
#'
#' @return a new tibble object with only the specified symbol in the symbol
#'   column.
#' @export
#'
#' @examples
filter_benchmark_by_symbol<-function(tibble_obj, symbol_value){
  op<-"=="
  return_tibble_obj<-filter(tibble_obj, do.call(op, list(symbol, symbol_value)))
  return(return_tibble_obj)
}

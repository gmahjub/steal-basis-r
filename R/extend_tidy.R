#' get_all_stocks_in_index
#'
#' this is a tidyquant implementation. Get all the components of an index and
#' pull stuff_to_get. stuff_to_get is defaulted to stock.prices, meaning the
#' component prices in a tibble time series.
#'
#' @param index_name name of the index to query compoenents, options for this
#'   value are in tq_index_options()
#' @param start_date the starting date of the time series data to pull for each
#'   index component
#' @param stuff_to_get defaults to stock.prices, but valid options are:
#'   financials, key.ratios, key.stats, dividends, splits, or any cobination of
#'   the latter in a vector format.
#'
#' @return a tibble object containing the time series price data of each stock
#'   in the specified index.
#' @export
#'
#' @examples
get_all_stocks_in_index<-function(index_name, start_date="2007-01-01", stuff_to_get=c("stock.prices")){
  # take the stocks in the index, and pull the prices
  # index name is as named in tq_index_optons()
  # stuff_to_get could be stock.prices, financials, key.ratios, key.stats, dividends, splits, or a combination of
  tibble_obj<-tq_index(index_name) %>% tq_get(get="stock.prices", from=start_date)
  return(tibble_obj)
}

#' get_stocks_groupby_sector
#'
#' Utilizes the tidyquant library, returns a tibble object with the data input
#' tibble object grouped by sector.The input tibble object most easily should be
#' returned from a tq_get() call on stock.prices. When multiple sectors are
#' present, this function groups them by sector if so needed in preparation for
#' mutation and/or further processing.
#'
#' @param tibble_obj a tibble object containing a column named sector.
#'
#' @return a tibble object grouped by sector.
#' @export
#'
#' @examples
get_stocks_groupby_sector<-function(tibble_obj){
  tibble_obj_by_sector<-tibble_obj %>% group_by(sector)
  return(tibble_obj_by_sector)
}

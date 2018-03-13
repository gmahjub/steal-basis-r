#' get_all_dividends_for_this_tibble
#'
#' Returns a tibble with columns: date, dividends, symbol. Basically a list of
#' all the dividends paid out for each distinch symbol in the tibble_obj passed
#' in as parameter. Most simply, the inputted tibble_obj will be a result of a
#' call to tq_index(). The input tibble must have a column named symbol. Stores
#' a copy of the dividend tibble in a csv file named <index_name>.dividends.csv.
#'
#' @param tibble_obj a result of a call to tq_index, most simply
#' @param index_name the name of the index for the constituents in the
#'   tibble_obj. See tq_index_options() for possibilities.
#' @param force_pull if FALSE, only pulls from remote repository if lcoal file
#'   is older than 1 day, when TRUE, always pulls from remote repository.
#' @param index_dir directory for specified index data, where files such as
#'   historical dividends file reside.
#'
#' @return tibble object with dividends and correspoinding dates paid out and
#'   the symbol of the dividend payer.
#' @export
#'
#' @examples
get_all_dividends_for_this_tibble<-function(tibble_obj, index_dir, index_name = "DOW", force_pull = FALSE){
  require(lubridate)
  path_to_local_div_file<-paste(index_dir, index_name, "/", sep = "")
  path_to_local_div_file<-paste(path_to_local_div_file, index_name, ".dividends.csv", sep = "")
  if (!force_pull) {
    if (file.exists(path_to_local_div_file)){
      # how old is the file? If older than 1 day, go to remote destination.
      last_modified<-file.info(path_to_local_div_file)$mtime
      # last modified is a POSIXct
      if (difftime(Sys.time(), last_modified, Sys.timezone(), units = c("days")) > 1){
        # then we must pull the dividend data from remote location
        force_pull = TRUE
      }
    } else {
      force_pull = TRUE
    }
  }
  message(paste("force pull is", force_pull, sep = " "))
  if (force_pull){
    distinct_symbols<-distinct(tibble_obj, symbol)
    dividend_dates<-tq_get(x = distinct_symbols$symbol[1], get = "dividends")
    dividend_dates<-add_column(dividend_dates, symbol = distinct_symbols$symbol[1])
    for (s in distinct_symbols$symbol[2:length(distinct_symbols$symbol)]){
      single <- try(tq_get(x=s, get = "dividends"))
      single<- add_column(single, symbol = s)
      dividend_dates<- try(rbind(dividend_dates, single))
    }
  } else {
    dividend_dates<-as.tibble(read.csv(path_to_local_div_file, header = TRUE, sep = ",", colClasses = c("character", "Date", "numeric","character")))
    dividend_dates<-dividend_dates[,2:ncol(dividend_dates)]
  }
  # write out the dividends table to local disk
  write.csv(dividend_dates, file = path_to_local_div_file)
  return(dividend_dates)
}

#' adjustOHLC_DividendWise_tibble_obj
#'
#' Does the same thing as AdjustOHLC(), but it is our in house function for
#' tibble objects. Calculates the dividend adjustment ratio and returns adusted
#' OHLC time series. This function is part of a workflow, must
#' get_dividends_for_this_tibble() first.
#'
#' @param tibble_obj
#' @param dividend_dates_tibble
#'
#' @return original tibble objec timeseries data with OHLC now adjusted for all
#'   dividends.
#' @export
#'
#' @examples
adjustOHLC_DividendWise_tibble_obj<-function(tibble_obj, dividend_dates_tibble){
  # dividend_dates_tibble comes from the a function call to get_all_dividends_for_this_tibble()
  distinct_symbols<-distinct(dividend_dates_tibble, symbol)
  distinct_symbols<-distinct_symbols$symbol
  new_tibble_obj<-left_join(tibble_obj, dividend_dates_tibble, by=c("date", "symbol"))
  new_tibble_obj<-new_tibble_obj %>% group_by(symbol) %>% mutate(dividends=lead(dividends)) %>% na.omit()
  new_tibble_obj<-new_tibble_obj %>% group_by(symbol) %>% mutate(single_div_adj_rat = (1 - as.numeric(dividends)/as.numeric(close)))
  final_div_rat_tibble_obj<-new_tibble_obj %>% filter(symbol == distinct_symbols[1]) %>% arrange(desc(date)) %>%
    mutate(final_div_rat = cumprod(single_div_adj_rat)) %>% select(c("symbol", "date", "final_div_rat"))
  full_tibble_obj <- tibble_obj %>% filter(symbol == distinct_symbols[1])
  full_tibble_obj<-left_join(full_tibble_obj, final_div_rat_tibble_obj, by=c("date", "symbol")) %>% na.locf(fromLast=TRUE)
  for (ds in distinct_symbols[2:length(distinct_symbols)]) {
    final_div_rat_tibble_obj<-new_tibble_obj %>% filter(symbol == ds) %>% arrange(desc(date)) %>%
      mutate(final_div_rat = cumprod(single_div_adj_rat)) %>% select(c("symbol", "date", "final_div_rat"))
    segment_tibble_obj <- tibble_obj %>% filter(symbol == ds)
    segment_tibble_obj<-left_join(segment_tibble_obj, final_div_rat_tibble_obj, by=c("date", "symbol")) %>% na.locf(fromLast=TRUE)
    full_tibble_obj<-try(rbind(full_tibble_obj, segment_tibble_obj))
  }
  full_tibble_obj<-full_tibble_obj %>% mutate(open = if_else(!is.na(final_div_rat), (as.numeric(open)*as.numeric(final_div_rat)), as.numeric(open)))
  full_tibble_obj<-full_tibble_obj %>% mutate(high = if_else(!is.na(final_div_rat), (as.numeric(high)*as.numeric(final_div_rat)), as.numeric(high)))
  full_tibble_obj<-full_tibble_obj %>% mutate(low = if_else(!is.na(final_div_rat), (as.numeric(low)*as.numeric(final_div_rat)), as.numeric(low)))
  full_tibble_obj<-full_tibble_obj %>% mutate(close = if_else(!is.na(final_div_rat), (as.numeric(close)*as.numeric(final_div_rat)), as.numeric(close)))
  return (full_tibble_obj)
}

get_single_dividend_history<-function(ticker){
  tq_get(ticker, get = "dividends", from = "2017-01-01")
}

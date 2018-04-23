#' get_all_dividends_for_this_tibble
#'
#' Returns a tibble with columns: date, dividends, symbol. Basically a list of
#' all the dividends paid out for each distinct symbol in the tibble_obj passed
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
get_all_dividends_for_this_tibble<-function(tibble_obj, index_dir=NA, index_name = NA, force_pull = FALSE){
  require(lubridate)
  if (!is.na(index_dir)){
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
  } else {
    # one off pull of dividends, no asssociation to any index-wide pull.
    force_pull = TRUE
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
  if (!is.na(index_dir)){
    write.csv(dividend_dates, file = path_to_local_div_file)
  }
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

#' adjustIbkrIntraday
#' 
#' IBrokers historical data uses xts formatted headers, ie. <ticker>.Close, <ticker>.Open, <ticker>.High, etc....
#' These headers are exactly what we would see if we did getSymbols() from QuantMod. In addition to OHLCV data,
#' IBrokers offers things like WAP, Count, and hasGaps, for TRADE type data (as opposed to BA Average).
#' This funciton handles looking for those columns. Use this function when you pull TRADE data. 
#' 
#' Let's just pass in the dividend Ratios into this funciton instead of calculating them again since we need
#' daily data to calculate them.
#'
#' @param tibble_obj 
#' @param dividend_dates_tibbl 
#'
#' @return
#' @export
#'
#' @examples
adjIbkrIntraday_helper<-function(tibble_obj){
  op_col<-Op(tibble_obj)
  op_col_unlisted<-var_colname_helper(op_col)
  hi_col<-Hi(tibble_obj)
  hi_col_unlisted<-var_colname_helper(hi_col)
  lo_col<-Lo(tibble_obj)
  lo_col_unlisted<-var_colname_helper(lo_col)
  cl_col<-Cl(tibble_obj)
  cl_col_unlisted<-var_colname_helper(cl_col)
  wap_col<-WAP(tibble_obj)
  wap_col_unlisted<-var_colname_helper(wap_col)
  full_tibble_obj<-tibble_obj %>% mutate(Adj.Open = if_else(!is.na(final_div_rat), (as.numeric(op_col_unlisted)*as.numeric(final_div_rat)), as.numeric(op_col_unlisted)))
  full_tibble_obj<-full_tibble_obj %>% mutate(Adj.High = if_else(!is.na(final_div_rat), (as.numeric(hi_col_unlisted)*as.numeric(final_div_rat)), as.numeric(hi_col_unlisted)))
  full_tibble_obj<-full_tibble_obj %>% mutate(Adj.Low = if_else(!is.na(final_div_rat), (as.numeric(lo_col_unlisted)*as.numeric(final_div_rat)), as.numeric(lo_col_unlisted)))
  full_tibble_obj<-full_tibble_obj %>% mutate(Adj.Close = if_else(!is.na(final_div_rat), (as.numeric(cl_col_unlisted)*as.numeric(final_div_rat)), as.numeric(cl_col_unlisted)))
  full_tibble_obj<-full_tibble_obj %>% mutate(Adj.WAP = if_else(!is.na(final_div_rat), (as.numeric(wap_col_unlisted)*as.numeric(final_div_rat)), as.numeric(wap_col_unlisted)))
  return(full_tibble_obj)  
}

var_colname_helper<-function(var_col){
  var_col<-unlist(var_col)
  var_col<-as.numeric(var_col)
  return (var_col)
}

get_single_dividend_history<-function(ticker, div_hist_start_date = "2017-01-01"){
  div_tibble<-tq_get(ticker, get = "dividends", from = div_hist_start_date)
  if (is.na(div_tibble)){
    return (NA)
  }
  div_tibble$symbol<-ticker
  return (div_tibble)
}

get_single_split_history<-function(ticker, split_hist_start_date = "2017-01-01"){
  split_tibble<-tq_get(ticker, get = "splits", from = split_hist_start_date)
  if (is.na(split_tibble)){
    return (NA)
  }
  split_tibble$symbol <- ticker
  return (split_tibble)
}

adjIbkrIntraday<-function(ticker, tibble_obj){
  div_hist_start_date<-as.Date(tibble_obj$BarTimeStamp[1], format = "%Y-%M-%D")
  tibble_obj$symbol<-ticker
  tibble_obj$date<-as.Date(tibble_obj$BarTimeStamp, format = "%Y-%M-%D")
  div_hist_start_date<-div_hist_start_date - 5
  single_div_history<-get_single_dividend_history(ticker, div_hist_start_date = div_hist_start_date)
  single_split_history<-get_single_split_history(ticker, split_hist_start_date = div_hist_start_date)
  px_series<-tq_get(ticker, get="stock.prices", from = div_hist_start_date)
  px_series$symbol <- ticker
  if (is.na(single_div_history) && is.na(single_split_history)){
    return (tibble_obj)
  } else if (is.na(single_split_history)){
    new_tibble_obj<-left_join(px_series, single_div_history, by=c("date", "symbol"))
    new_tibble_obj<-new_tibble_obj %>% group_by(symbol) %>% mutate(dividends=lead(dividends)) %>% na.omit()
    new_tibble_obj<-new_tibble_obj %>% group_by(symbol) %>% mutate(single_div_adj_rat = (1 - as.numeric(dividends)/as.numeric(close)))
    final_div_rat_tibble_obj<-new_tibble_obj  %>% arrange(desc(date)) %>%
      mutate(final_div_rat = cumprod(single_div_adj_rat)) %>% select(c("symbol", "date", "final_div_rat"))
    full_tibble_obj<-left_join(tibble_obj, final_div_rat_tibble_obj, by=c("date", "symbol")) %>% 
      replace(., TRUE, lapply(., na.locf, na.rm = FALSE, fromLast = TRUE))
    res_tibble<-adjIbkrIntraday_helper(full_tibble_obj)
    res_tibble[, "BarTimeStamp"] <- as.POSIXct(res_tibble[["BarTimeStamp"]])
    cols_to_drop<-c("symbol", "date", "final_div_rat")
    res_tibble<-res_tibble %>% select_(.dots = setdiff(names(.), cols_to_drop))
  } else if (is.na(single_div_history)){
    
  }
}

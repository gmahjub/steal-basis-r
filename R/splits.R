#' get_all_splits_for_this_tibble
#'
#' Same as get_all_dividends_for_this_tibble except in this case returns splits
#' for the symbols in the inputted tibble object.
#'
#' @param tibble_obj
#'
#' @return tibble object containing dates, size of split on said date, and the
#'   symbol of ticker that split.
#' @export
#'
#' @examples
get_all_splits_for_this_tibble<-function(tibble_obj){
  ## Don't think we really need two for loops here but whatever. Slow as fug.
  distinct_symbols<-distinct(tibble_obj, symbol)
  split_dates<-tibble()
  for (i in 1:length(distinct_symbols$symbol)){
    single<-tq_get(x=distinct_symbols$symbol[i], get="splits")
    if (is.na(single[1])){
      single<-tibble(date=tibble_obj$date[1], splits=NA)
      single<-add_column(single, symbol=distinct_symbols$symbol[i])
      split_dates<-try(rbind(split_dates, single))
      next
    }
    single <- add_column(single, symbol = distinct_symbols$symbol[i])
    split_dates<-try(rbind(split_dates, single))
  }
  split_dates_tibble<-split_dates
  return(split_dates_tibble)
}

#' adjustOHLC_Splitwise_tibble_obj
#'
#' Currently, not used because all of the Yahoo finance data is split adjusted.
#'
#' @param tibble_obj
#' @param split_dates_tibble
#'
#' @return
#' @export
#'
#' @examples
adjustOHLC_SplitWise_tibble_obj<-function(tibble_obj, split_dates_tibble){
  new_tibble_obj<-left_join(tibble_obj, split_dates_tibble, by=c("date", "symbol"))
  distinct_symbols<-distinct(split_dates_tibble, symbol)
  distinct_symbols<-distinct_symbols$symbol
  full_tibble_obj <- new_tibble_obj %>% filter(symbol==distinct_symbols[1]) %>% na.locf(fromLast=TRUE)
  for (ds in distinct_symbols[2:length(distinct_symbols)]){
    segment_tibble_obj<-new_tibble_obj %>% filter(symbol==ds) %>% na.locf(fromLast=TRUE)
    full_tibble_obj<-try(rbind(full_tibble_obj, segment_tibble_obj))
  }
  full_tibble_obj<-full_tibble_obj %>% mutate(open = if_else(!is.na(splits), (as.numeric(open)*as.numeric(splits)), as.numeric(open)))
  full_tibble_obj<-full_tibble_obj %>% mutate(high = if_else(!is.na(splits), (as.numeric(high)*as.numeric(splits)), as.numeric(high)))
  full_tibble_obj<-full_tibble_obj %>% mutate(low = if_else(!is.na(splits), (as.numeric(low)*as.numeric(splits)), as.numeric(low)))
  return (full_tibble_obj)
}

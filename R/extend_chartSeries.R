extend_chartSeries<-function(ticker, from_date, to_date = NA){
  if (is.na(to_date)) { getSymbols(ticker, from = from_date)
  } else { getSymbols(ticker, from = from_date, to = to_date) }
  get(ticker) %>% chartSeries()
}

#' extend_chartSeries_BBands
#'
#' @param ticker
#' @param subset_year_month_day
#'
#' @return
#' @export
#'
#' @examples
extend_chartSeries_BBands<-function(ticker, subset_year_month_day = NA){
  get(ticker) %>% chartSeries(TA='addBBands();
                                  addBBands(draw="p");
                                  addVo()',
                              subset = subset_year_month_day,
                              theme = "white")
}

#' extend_chartSeries_MACD
#'
#' @param ticker
#' @param subset_year_month_day
#'
#' @return
#' @export
#'
#' @examples
extend_chartSeries_MACD<-function(ticker, subset_year_month_day = NA){
  get(ticker) %>% chartSeries(TA = 'addVo();
                                    addMACD()',
                              subset = subset_year_month_day,
                              theme="white")
}

#' extend_chartSeries_BBands_MACD
#'
#' @param ticker
#' @param subset_year_month_day
#'
#' @return
#' @export
#'
#' @examples
extend_chartSeries_BBands_MACD<-function(ticker, subset_year_month_day = NA){
  get(ticker) %>% chartSeries(TA = 'addBBands();
                                    addBBands(draw="p");
                                    addVo();
                                    addMACD()',
                              subset = subset_year_month_day,
                              theme="white")
}

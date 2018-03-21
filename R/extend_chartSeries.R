extend_chartSeries<-function(ticker, from_date, to_date = Sys.Date()){
  ticker <- c(ticker)
  yahoo_Main_retrieve_and_write(ticker, NA, start_date = from_date, end_date = to_date, write_file = FALSE)
  get(ticker) %>% chartSeries(theme = chartTheme("white"))
  xts_obj <- get(ticker)
  addTA(OBV(Ad(xts_obj), Vo(xts_obj)))
}

extend_chartSeries_OBV<-function(ticker, from_date, to_date = Sys.Date()){
  ticker <- c(ticker)
  yahoo_Main_retrieve_and_write(ticker, NA, start_date = from_date, end_date = to_date, write_file = FALSE)
  get(ticker) %>% chartSeries(TA = 'addOBV()')
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

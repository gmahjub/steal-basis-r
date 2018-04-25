extend_chartSeries<-function(xts_obj){
  xts_obj %>% chartSeries(theme = chartTheme("white"))
  addTA(OBV(Ad(xts_obj), Vo(xts_obj)))
}

extend_chartSeries_OBV<-function(xts_obj, subset_year_month_day = NA){
  get(ticker) %>% chartSeries(TA = 'addOBV()',
                              subset = subset_year_month_day,
                              theme = "white")
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
extend_chartSeries_BBands<-function(xts_obj, subset_year_month_day = NA){
  xts_obj %>% chartSeries(TA='addBBands();
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
  xts_obj %>% chartSeries(TA = 'addVo();
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
extend_chartSeries_BBands_MACD<-function(xts_obj, subset_year_month_day = NA){
  xts_obj %>% chartSeries(TA = 'addBBands();
                                addBBands(draw="p");
                                addVo();
                                addMACD()',
                                subset = subset_year_month_day,
                                theme="white")
}

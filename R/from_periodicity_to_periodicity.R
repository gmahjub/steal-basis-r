#' from_periodicity_to_periodicity
#'
#' Convert periodicity of OHLC data. xts_obj is most simply, a result of a call
#' to getSymbols(). Assumes the column headers are named as getSymbols() would
#' name them, <ticker>.Open, <ticker>.Close, <ticker>.High, <ticker>.Low, etc...
#' This function generalizes the xts functions to.<some periodicity>.
#'
#' @param xts_obj
#' @param to_periodicity default to "weekly", could be "monthly", "quarterly",
#'   "yearly", etc...
#' @param xts_obj_name default to NA, in which case xts_obj name is used,
#'   usually this would be the ticker
#'
#' @return
#' @export
#'
#' @examples
from_periodicity_to_periodicity<-function(xts_obj, to_periodicity = "weekly", xts_obj_name=NA){
  # takes an xts object of some periodicity, and converts it to a lower periodicity
  # ex. daily -> weekly
  to_func<-paste("to.", to_periodicity, sep = "")
  assign("to_func", get(to_func))
  if (is.na(xts_obj_name)){
    names <- colnames(xts_obj)
    xts_obj_name<-unlist(strsplit(names[1], "\\."))[1]
  }
  res<-do.call(to_func, list(xts_obj, name=xts_obj_name))
  return(res)
}

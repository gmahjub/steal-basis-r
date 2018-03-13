#' get_rolling_rate_of_change
#'
#' Uses the ROC function from TTR library. ROC is rate of change, and is
#' returned as a percentage value.
#'
#' @param tibble_obj simply, a result of a call to tq_index()
#'
#' @return a new tibble with only the "Rolling" 1 Day return.
#' @export
#'
#' @examples
get_rolling_rate_of_change<-function(tibble_obj, price_type = "adjusted", periodicity = "daily",
                                num_periods = 1, annualize = FALSE){
  pl<-toupper(substr(periodicity, 1, 1))
  col_header<-paste("ROC.", num_periods, pl, sep = "")
  tibble_returns_obj<-tibble_obj %>% group_by(symbol) %>% tq_transmute_(select = price_type,
                                                                        mutate_fun = c("ROC"),
                                                                        n=num_periods,
                                                                        type="continuous",
                                                                        col_rename="temp")
  if (annualize){
    if (tolower(periodicity) == "daily"){
      scale <- 252/num_periods
    } else if (tolower(periodicity) == "weekly"){
      scale <- 52/num_periods
    } else if (tolower(periodicity) == "monthly"){
      scale <- 12/num_periods
    } else if (tolower(periodicity) == "quarterly"){
      scale <- 4/num_periods
    } else if (tolower(periodicity) == "yearly"){
      scale <- 1/num_periods
    }
    tibble_returns_obj<-tibble_returns_obj %>% group_by(symbol) %>% mutate(temp = temp*sqrt(scale)) %>%
      `colnames<-`(c("symbol", "date", paste(col_header, ".Annualized", sep = "")))
  } else {
    tibble_returns_obj <- tibble_returns_obj %>% `colnames<-`(c("symbol", "date", col_header))
  }
  return(tibble_returns_obj)
}



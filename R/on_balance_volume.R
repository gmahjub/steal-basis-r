on_balance_volume_intraday <- function(tibble_obj){
  # calculate the on-balance volume intraday
  volume_pos_return<-0
  volume_neg_return<-0

  daily_returns<-tibble_obj %>% tq_transmute(select=adjusted, mutate_fun = periodReturn, period="daily",
                                             type="log", col_rename = "DailyLog.return")
  return(daily_returns)

}

on_balance_volume_intraday_xts<-function(xts_obj){

}

calc_rolling_obv<-function(ticker, win_size = 2){
  xts_obj<-get(ticker)
  diff_col<-diff(Ad(xts_obj))
  colnames(diff_col)<- paste(ticker, "Px_Chg_Adj_Cl", sep = ".")
  volume_col<-Vo(xts_obj)
  y<-ifelse(diff_col > 0, volume_col, volume_col*-1.0)
  colnames(y)<-paste(ticker, "volume_ob_adj", sep = ".")
  rolling_obv <- rollapply(y, width = win_size, FUN = sum)
  colnames(rolling_obv)<-paste(ticker, "rolling_obv", sep = ".")
  return(rolling_obv)
}
create_obv_data<-function(ticker, win_size = 2, period = "days"){
  xts_obj<-get(ticker)
  diff_col<-diff(Ad(xts_obj))
  colnames(diff_col)<- paste(ticker, "Px_Chg_Adj_Cl", sep = ".")
  volume_col<-Vo(xts_obj)
  y<-ifelse(diff_col > 0, volume_col, volume_col*-1.0)
  colnames(y)<-paste(ticker, "volume_ob_adj", sep = ".")
}

cal_period_obv<-function(ticker){
  xts_obj<-get(ticker)
  
}

calc_period_obv_helper<-function(xts_obj){
  xts_obj<-get(ticker)
  diff_col<-diff(Ad(xts_obj))
  volume_col<-Vo(xts_obj)
  
  
}

calc_volume_moving_avg<-function(ticker, win_size = 30){
  xts_obj<-get(ticker)
  volume_col<-Vo(xts_obj)
  rolling_avg_volume<-rolling_volume<-rollapply(volume_col, width = win_size, FUN  = mean)
  colnames(rolling_avg_volume)<-paste(ticker, win_size, "period.Avg.Voume", sep = ".")
  return(rolling_avg_volume)
}
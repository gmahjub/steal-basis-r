on_balance_volume_tidy<-function(tibble_obj){
  tibble_obj
}

on_balance_volume_xts<-function(ticker, xts_obj=NA){
  if (is.na(xts_obj)){
    xts_obj<-get(ticker)
  }
  return(obv)
}

calc_rolling_obv<-function(ticker, win_size = 2, period = "days"){
  xts_obj<-get(ticker)
  diff_col<-diff(Ad(xts_obj))
  colnames(diff_col)<- paste(ticker, "Diff_Adj_Cl", sep = ".")
  xts_obj<-merge(xts_obj, diff_col)
  price_col_nm<-paste(ticker, "Diff_Adj_Cl", sep=".")
  vol_col_nm<-paste(ticker, "Volume", sep = ".")
  y<-ifelse(xts_obj[,price_col_nm] > 0, xts_obj[,vol_col_nm], xts_obj[,vol_col_nm]*-1.0)
  colnames(y)<-paste(ticker, "volume_obv_adj", sep = ".")
  xts_obj<-merge(xts_obj, y)
  return(xts_obj)
  #period.apply(xts_obj, INDEX=ep, FUN = function(z) ifelse(z[price_col_nm] > 0, y<-z[vol_col_nm], y<-z[vol_col_nm]*-1) merge(z))
}

calc_rolling_obv_helper<-function(x, ticker){
  price_col_nm<-paste(ticker, "Diff_Adj_Cl", sep=".")
  vol_col_nm<-paste(ticker, "Volume", sep = ".")
  if (x[price_col_nm] > 0){
    y<-x[vol_col_nm]
  } else {
    y<-x[vol_col_nm]*-1
  }
  colnames(y)<-"volume_obv_adj"
  x<-merge(x, y)
  return(x)
  
}
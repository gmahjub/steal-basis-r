calc_negative_volume_index<-function(ticker){
  neg_volume_index<- 100.0
  xts_obj<-get(ticker)
  volume_diff<-diff(Vo(xts_obj))
  nvi_init<-100
  y<-merge(volume_diff, nvi_init)
  y<-merge(y, Ad(xts_obj))
  y$nvi_init[1]<-100.0
  colnames(y)<-c("volume_diff", "NVI", "Adj_Cl")
  ep <-endpoints(y, 'days')
  helper_ret_val<-period.apply(y, INDEX=ep, FUN = NVI_helper)
  return(y)
}

NVI_helper<-function(x){
  #return_val<-ifelse(x$volume_diff>=0, lag(x[,"NVI"]), ((x[,"Adj_Cl"] - lag(x[,"Adj_Cl"]))/lag(x[,"Adj_Cl"]) + 1)*lag(x[,"NVI"]))
  #print (return_val)
  #return (return_val)
  x$NVI<-x$NVI - x$Adj_Cl
}

calc_positive_volume_index<-function(ticker){
  # same as above but for positive volume changes period over period.
}
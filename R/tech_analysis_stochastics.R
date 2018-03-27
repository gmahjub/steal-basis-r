calc_stochastics<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType=list(list(EMA, wilder = TRUE), list(SMA), list(SMA)), 
                           bounded = TRUE, smooth = 1){
  # fast %D = slow %K - they are equal, that's why we don't see a return value for slow %K
  # maType here must specify 3 types of MA's, the first for the FastD, the second for the SlowD, and the third for the internal smoothing
  #  of the C-L and H-L values to calculated FastK.
  xts_obj<-get(ticker)
  hlc_px_series<-HLC(xts_obj)
  stoch_vals<-stoch(hlc_px_series, nFastK = nFastK, nFastD = nFastD, nSlowD = nSlowD, maType = maType, bounded = bounded, smooth = smooth)
  return (stoch_vals)
}

calc_fast_stochastics_percentK<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = TRUE)),
                                         bounded = TRUE, smooth = 1){
  # TTR::stoch()
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastK)
}

calc_fast_stochastics_percentD<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = TRUE)),
                                         bounded = TRUE, smooth = 1){
  # TTR:stoch()
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastD)
}

calc_slow_stochastics_percentK<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = TRUE)),
                                         bounded = TRUE, smooth = 1){
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$fastD)
}

calc_slow_stochastics_percentD<-function(ticker, nFastK = 14, nFastD = 3, nSlowD = 3, maType = list(list(EMA), list(SMA), list(EMA, wilder = TRUE)),
                                         bounded = TRUE, smooth = 1){
  # TTR:stoch()
  stoch_vals<-calc_stochastics(ticker, nFastK, nFastD, nSlowD, maType, bounded, smooth)
  return(stoch_vals$slowD)
}

calc_williams_percentR<-function(ticker){
  xts_obj<-get(ticker)
}

calc_chaikin_volatility<-function(ticker){
  xts_obj<-get(ticker)
}

calc_volatility<-function(ticker){
  xts_obj<-get(ticker)
}


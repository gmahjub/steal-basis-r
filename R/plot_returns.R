plot_log_returns_hist<-function(ticker, price_type_func = "Ad", num_bins = 100, alpha = 0.5){
  log_returns <- do.call(price_type_func, list(get(ticker))) %>% dailyReturn(type = "log")
  names(log_returns) <- paste(ticker, ".Log.Returns", sep = "")
  log_returns %>% ggplot(aes_(x = as.name(names(log_returns)) )) +
    geom_histogram(bins = num_bins) +
    geom_density() +
    geom_rug(alpha=alpha)
}

quantile_distribution

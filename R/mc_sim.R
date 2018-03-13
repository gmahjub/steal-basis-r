quantile_dist_log_returns<-function(log_returns, probs = c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, 0.995)){
  dist_log_returns <- log_returns %>% quantile(probs = probs, na.rm = TRUE)
  return (dist_log_returns)
}

mean_log_returns<-function(log_returns){
  mean_log_returns <- mean(log_returns, na.rm = TRUE) %>% exp()
  return (mean_log_returns - 1)
}

sd_log_returns<-function(log_returns){
  sd_log_returns <- sd(log_returns, na.rm = TRUE) %>% exp()
  return (sd_log_returns)
}

simulate<-function(ticker, sim_size, mean_log_return, sd_log_return, price_type = "Adjusted"){
  sim_nun<-1:sim_size
  the_data<-get(ticker)
  col_nm <- paste(ticker, price_type, sep = ".")
  the_col <- the_data[,col_nm]
  # just initialize the price to the last price
  price_init<-the_col[[nrow(the_col)]]
  # simulate prices
  set.seed(386)
  price <- c(price_init, rep(NA, N-1))
  for (i in 2:N) {
    price[i] <- price[i-1] * exp(rnorm(1, mean_log_return, sd_log_return))
  }
  price_sim <- cbind(sim_num, price) %>% as_tibble()
  # visualize
  return (price_sim)
}

plot_simulation<-function(simulation, sim_size, price, title="Monte Carlo Simulation Plot"){
  sim_num <- 1:sim_size
  simulation %>% ggplot(aes(sim_num, price )) +
    geom_line() +
    ggtitle(str_c(title))
}

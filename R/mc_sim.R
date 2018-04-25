calc_quantiles<-function(x, probs = c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, 0.995)){
  y <- x %>% quantile(probs = probs, na.rm = TRUE)
  return (y)
}

mean_log_returns<-function(log_returns){
  mean_log_returns <- mean(log_returns, na.rm = TRUE)
  return (mean_log_returns)
}

sd_log_returns<-function(log_returns){
  sd_log_returns <- sd(log_returns, na.rm = TRUE)
  return (sd_log_returns)
}

log2realReturns<-function(log_returns){
  return (log_returns %>% exp())
}

#' simulate
#'
#' Monte Carlo Simulation. This is not as useful as the next method, which does
#' 100's,1000's of simulations. This does only 1 simulation, and is only for
#' illustration purpose.
#'
#' @param ticker
#' @param sim_size
#' @param mean_log_return
#' @param sd_log_return
#' @param price_type
#'
#' @return
#' @export
#'
#' @examples
simulate<-function(ticker, sim_size, mean_log_return, sd_log_return, xts_obj = NULL, price_type = "Adjusted"){
  sim_num<-1:sim_size
  if (is.null(xts_obj)){
    the_data<-get(ticker)
  } else {
    the_data <- xts_obj
  }
  col_nm <- paste(ticker, price_type, sep = ".")
  the_col <- the_data[,col_nm]
  price_init<-the_col[[nrow(the_col)]]
  set.seed(386)
  price <- c(price_init, rep(NA, sim_size-1))
  for (i in 2:sim_size) {
    price[i] <- price[i-1] * exp(rnorm(1, mean_log_return, sd_log_return))
  }
  price_sim <- cbind(sim_num, price) %>% as_tibble()
  # visualize
  return (price_sim)
}

monte_carlo<-function(ticker, sim_size, mc_runs, mean_log_return, sd_log_return, xts_obj = NULL, price_type = "Adjusted"){
  N<-sim_size
  M<-mc_runs
  mu<-mean_log_return
  sigma<-sd_log_return
  T_t<-1:sim_size
  if(is.null(xts_obj)){
    the_data<-get(ticker)
  } else {
    the_data<-xts_obj
  }
  col_nm<-paste(ticker, price_type, sep = ".")
  the_col<-the_data[, col_nm]
  price_init<-the_col[[nrow(the_col)]]
  set.seed(123)
  monte_carlo_matrix<-matrix(nrow = N, ncol = M)
  for (j in 1:M) {
    monte_carlo_matrix[[1, j]] <- price_init
    for (i in 2:N){
      monte_carlo_matrix[[i,j]] <- monte_carlo_matrix[[i-1, j]] * exp(rnorm(1, mu, sigma))
    }
  }
  price_sim <-cbind(T_t, monte_carlo_matrix) %>% as_tibble()
  nm <- str_c("Sim.", seq(1,M))
  nm <- c("T_t", nm)
  names(price_sim) <- nm
  price_sim<-price_sim %>% gather(key = "Simulation", value = "Stock.Price", -(T_t))
  #price_sim %>% ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  #  geom_line(alpha = 0.1) + ggtitle(str_c(ticker, ": ", M,
  #                                         " Monte Carlo Simulations for Prices Over ", N, "Trading Days"))
  return (price_sim)
}

plot_monte_carlo<-function(ticker, price_sim, sim_size, mc_runs, title = "Monte Carlo Simulation Plot"){
  title<-paste(ticker, ": ", mc_runs, " Monte Carlo Simulation for Prices Over ", sim_size, " Trading Days", sep = "")
  price_sim %>% ggplot(aes(x = T_t, y = Stock.Price, Group = Simulation)) + 
    geom_line(alpha = 0.1) + ggtitle(title)
}

monte_carlo_return_dist<-function(price_sim){
  end_of_sim_prices<-price_sim %>% filter(T_t == max(T_t))
  probs<-c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, .995)
  dist_end_stock_prices<-calc_quantiles(end_of_sim_prices$Stock.Price, probs = probs)
  return (dist_end_stock_prices %>% round(2) )
}

plot_simulation<-function(ticker, simulation, sim_size, price, title="Single Simulation Plot"){
  title<-paste(ticker, ": Simulated Prices for ", sim_size, " Trading Periods" )
  sim_num <- 1:sim_size
  simulation %>% ggplot(aes(sim_num, price )) +
    geom_line() +
    ggtitle(str_c(title))
}

#' plot_log_returns_hist
#'
#' @param ticker 
#' @param price_type_func 
#' @param num_bins 
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
plot_log_returns_histogram<-function(ticker, price_type_func = "Ad", num_bins = 100, alpha = 0.5){
  log_returns <- get_log_returns(ticker, price_type_func = price_type_func)
  log_returns %>% ggplot(aes_(x = as.name(names(log_returns)) )) +
    geom_histogram(bins = num_bins) +
    geom_density() +
    geom_rug(alpha=alpha)
}

get_log_returns<-function(ticker, xts_obj = NULL, price_type_func = "Ad"){
  if (is.null(xts_obj)){
    xts_obj<-get(ticker)
  }
  log_returns<-do.call(price_type_func, list(xts_obj)) %>% dailyReturn(type = "log")
  names(log_returns) <-paste(ticker, ".Log.Returns", sep = "")
  return(log_returns)
}


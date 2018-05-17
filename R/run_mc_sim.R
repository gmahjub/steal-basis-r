## run mc_sim : Monte Carlo Simulation
ticker<-"YNDX"
ticker<-"INVA"
getYahooDaily_xts(ticker, yahoo_stock_prices_dir = yahoo_stock_prices_dir, from_date = "2007-01-01")
log_returns<-get_log_returns(ticker, xts_obj = NULL, price_type_func = "Ad")

get(ticker) %>% chartSeries(TA='addBBands();
                            addBBands(draw="p");
                            addVo();
                            addMACD()',
                            subset='2018',
                            theme = "white")

## plot the histogram
plot_log_returns_histogram(ticker, price_type_func = "Ad", num_bins = 100, alpha = 0.5)

# check if the returns distribution is normal
# calc mean and sd of returns
mean_log_rets<-mean_log_returns(log_returns)
message(paste("Mean of Log Returns: ", mean_log_rets, sep = ""))
sd_log_rets<-sd_log_returns(log_returns)
message(paste("Sd of Log Returns: ", sd_log_rets, sep = ""))
dist_log_returns<-calc_quantiles(log_returns)
mean_real_ret<-log2realReturns(mean_log_rets)
message(paste("Mean Real Return: ", mean_real_ret, " %", sep = ""))

# 1 simulation
how_many_sims<-1
num_time_periods_to_simulate<-1000
simulation_res<-simulate(ticker, sim_size = num_time_periods_to_simulate, mean_log_return = mean_log_rets, sd_log_return = sd_log_rets, xts_obj = NULL, price_type = "Adjusted")
plot_simulation(ticker, simulation = simulation_res$price_sim, sim_size = num_time_periods_to_simulate, price = simulation_res$price)

# Monte Carlo (many) Simulations
how_many_sims<- 1000
monte_carlo_res<-monte_carlo(ticker, sim_size = num_time_periods_to_simulate, mc_runs = how_many_sims, mean_log_return = mean_log_rets, sd_log_return = sd_log_rets, xts_obj = NULL, price_type = "Adjusted" )
plot_monte_carlo(ticker, price_sim = monte_carlo_res, sim_size = num_time_periods_to_simulate, mc_runs = how_many_sims)
mc_return_dist<-monte_carlo_return_dist(monte_carlo_res)
message(paste("Median (Most Likely) Price - Monte Carlo Simulation - ", ticker, " - ", mc_return_dist["50%"], sep = "" ))
CAGR_res<-calc_CAGR(ticker, num_time_periods_to_simulate = num_time_periods_to_simulate, mc_return_dist = mc_return_dist)
CAGR_hist<-(CAGR_res$CAGR_Historical %>% round(4))
CAGR_sim<-(CAGR_res$CAGR_Sim %>% round(4))
message(paste("Historical CAGR - ", ticker, " : ", CAGR_hist*100.0, " %", sep = ""))
message(paste("Simulated CAGR - ", ticker, " : ", CAGR_sim*100.0, " %", sep = ""))

run_batch_mc_sim<-function(idx_nm = "DOW", start_date = "2007-01-01"){
  index_holdings<-tq_index(idx_nm)
  rets_px_nested_tibble<- index_holdings %>% mutate(
    stock.prices = map(symbol,
                       function (.x) getYahooDaily_xts(.x,
                                                       yahoo_stock_prices_dir = yahoo_stock_prices_dir,
                                                       from_date = start_date,
                                                       return_format = "tibble")),
    log.returns = map(stock.prices,
                      function (.x) get_daily_log_returns(.x)),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )
  return (rets_px_nested_tibble)
}

plot_batch_mc_sim_results<-function(rets_px_nested_tibble){
  plot_ly(data = rets_px_nested_tibble,
          type = "scatter",
          mode = "markers",
          x = ~ sd.log.returns,
          y = ~ mean.log.returns,
          color = ~ n.trade.days,
          colors = "Blues",
          size = ~ n.trade.days,
          text = ~ str_c("<em>", company, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "Sector: ", sector, "<br>",
                         "No. of Trading Days: ", n.trade.days),
          marker = list(opacity = 0.8,
                        symbol = 'circle',
                        sizemode = 'diameter',
                        sizeref = 4.0,
                        line = list(width = 2, color = '#FFFFFF'))
          ) %>%
    layout(title = 'S&amp;P500 Analysis: Stock Risk vs Reward',
           xaxis = list(title = 'Risk/Variability (Stdev Log Returns)',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
           yaxis = list(title = 'Reward/Growth (Mean Log Returns)',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
           margin = list(l = 100,
                         t = 100,
                         b = 100),
           font = list(color = '#FFFFFF'),
           paper_bgcolor = 'rgb(0,0,0)',
           plot_bgcolor = 'rgb(0,0,0)')
}

# expand this analysis to all stocks in an index
ticker_tibble<-getYahooDaily_xts(ticker, yahoo_stock_prices_dir = yahoo_stock_prices_dir, from_date = "2013-01-01", return_format = "tibble")
log_returns_tibble<-get_daily_log_returns(ticker_tibble)


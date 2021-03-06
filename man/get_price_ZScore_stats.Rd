% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zscore_stats_funcs.R
\name{get_price_ZScore_stats}
\alias{get_price_ZScore_stats}
\title{get_price_ZScore_stats}
\usage{
get_price_ZScore_stats(tibble_obj, periodicity = "daily", window_size = 5,
  annualize = FALSE, scale = 252)
}
\arguments{
\item{tibble_obj}{most simply, the result of a call to tq_index(), requring a
column named "adjusted" to function properly.}
}
\value{
the original tibble, plus appended columns for mean, asd, hi.95ci,
  lo.95ci, asr
}
\description{
Calculate the Zscore stats (mean, sd, 95% CI's) for a tibble object where the
tibble object could be return values or raw price data. This function
calculates a rolling window_size Zscore dataset using the helper function
zscore_stats_helper(). It uses adjusted closing prices for this calculation.
}

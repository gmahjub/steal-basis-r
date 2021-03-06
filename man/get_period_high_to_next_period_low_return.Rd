% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/returns.R
\name{get_period_high_to_next_period_low_return}
\alias{get_period_high_to_next_period_low_return}
\title{get_period_high_to_next_period_low_return}
\usage{
get_period_high_to_next_period_low_return(period_OHLCVA_tibble,
  periodicity = "daily")
}
\arguments{
\item{period_OHLCVA_tibble}{a tibble of the speicified periodicity. Most
simply, this should be a result of a call to get_period_OHLCVA()}

\item{periodicity}{options are: daily, weekly, monthly, yearly}
}
\value{
a tibble object containing period high to next period low returns.
}
\description{
Calculates the return between the high at time t, to the low at time t+1.
Depending on what the periodicity is, time is in days, weeks, months, years.
Takes as input a tibble object with the min and the max of each price type
(op, adcl, hi, lo).  The min_max_ohlc_tibble is returned from the function
get_append_hi_to_lo_period_returns(). The periodicity arguement here should
match the periodicty of the apply in get_append_hi_to_lo_period_returns().
}

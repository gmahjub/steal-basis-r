% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extend_yahoo_quantmod.R
\name{yahoo_Main_retrieve_and_write}
\alias{yahoo_Main_retrieve_and_write}
\title{yahoo_Main_retrieve_and_write}
\usage{
yahoo_Main_retrieve_and_write(tickers, yahoo_stock_prices_dir,
  start_date = "2017-11-01")
}
\arguments{
\item{tickers}{list of tickers to retrieve data objects (xts/zoo) for.}

\item{yahoo_stock_prices_dir}{yahoo stock price data directory.}

\item{start_date}{start_date of the time series data to retrieve.}
}
\value{
a lit of ticker
}
\description{
Main function for retrieving time series data from yahoo finance.
}

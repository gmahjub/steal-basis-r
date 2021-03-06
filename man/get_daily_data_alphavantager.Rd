% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/secure_api_dataAccess.R
\name{get_daily_data_alphavantager}
\alias{get_daily_data_alphavantager}
\title{get_daily_data_alphavantager}
\usage{
get_daily_data_alphavantager(ticker)
}
\arguments{
\item{ticker}{ticker as string}
}
\value{
a tibble object containing the time series of the specified ticker.
}
\description{
Uses the av_get() function of the alphavantage api to pull daily periodicity
time series data. Pulls the full data, outputsize set to "full" such that all
the data in the remote alphavantage db is pulled.
}

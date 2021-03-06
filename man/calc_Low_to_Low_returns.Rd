% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extend_quantmod.R
\name{calc_Low_to_Low_returns}
\alias{calc_Low_to_Low_returns}
\title{calc_Low_to_Low_returns}
\usage{
calc_Low_to_Low_returns(ticker)
}
\arguments{
\item{ticker}{the ticker string representing the xts time series data object}
}
\value{
returns nothing
}
\description{
calculates the returns of the close at t and the close at t-1. Uses the
Return.calculate functionality. Creates a new column named "LoLoDailyRets"
and appends it onto existing xts object and assigns this new object to memory
with name "ticker".
}

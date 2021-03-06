% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_data_access.R
\name{get_holdings_filtered}
\alias{get_holdings_filtered}
\title{get_holdings_filtered}
\usage{
get_holdings_filtered(holdings_tibble_obj, filter_column_name, filter_operator,
  filter_value)
}
\arguments{
\item{holdings_tibble_obj}{tibble object with index holdings, tickers, and possibly more information.}

\item{filter_column_name}{the column to filter on}

\item{filter_operator}{the filter operator : "==", ">", "<", etc...}

\item{filter_value}{the value to filter for.}
}
\value{
a tibbe obj, filtered based on condition.
}
\description{
filters the inputted tilbble based on the conditions specified in paramters.
For example, if filter_column name is "Asset.Class" and filter operator is
"==", and the filter condition is "Equity", the we will returned all the rows
in the input tibble object that has the Asset.Class column equal to "Equity".
}

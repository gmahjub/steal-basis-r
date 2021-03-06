% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extend_tidy.R
\name{get_stocks_groupby_sector}
\alias{get_stocks_groupby_sector}
\title{get_stocks_groupby_sector}
\usage{
get_stocks_groupby_sector(tibble_obj)
}
\arguments{
\item{tibble_obj}{a tibble object containing a column named sector.}
}
\value{
a tibble object grouped by sector.
}
\description{
Utilizes the tidyquant library, returns a tibble object with the data input
tibble object grouped by sector.The input tibble object most easily should be
returned from a tq_get() call on stock.prices. When multiple sectors are
present, this function groups them by sector if so needed in preparation for
mutation and/or further processing.
}

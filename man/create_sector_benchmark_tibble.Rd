% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark_tidy.R
\name{create_sector_benchmark_tibble}
\alias{create_sector_benchmark_tibble}
\title{create_sector_benchmark_tibble}
\usage{
create_sector_benchmark_tibble(tibble_obj, index_name = "DOW")
}
\arguments{
\item{tibble_obj}{most simply, a result of a call to tq_index()}

\item{index_name}{options are: tq_index_options() defaulted to "DOW". Should
match the index used when input tibble_obj was created.}
}
\value{
a tibble object with (benchmark etf ticker, sector
  description) pairs
}
\description{
This function takes a inputted tibble objectk, which results from a call to
tq_index(). tq_index takes as a parameter an index name (where valid index
names can be found with a call to tq_index_options). The returned tibble from
tq_index("DOW") for example must have a sector column. This function is part
of a workflow which involves the creation of the inputted tibble object via
get_all_stocks_in_index(). This function returns a tibble with each row being
a (benchmark etf, sector) pair. For example, (XLY, Consumer
Discretinary),(XLE, Energy), (DIA, ZZZ.Index). Notice the index benchmark etf
is included here, and to ensure it shows up last in the tibble, the sector is
appended with "ZZZ".
}

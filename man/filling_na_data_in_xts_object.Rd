% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extend_quantmod.R
\name{filling_na_data_in_xts_object}
\alias{filling_na_data_in_xts_object}
\title{filling_na_data_in_xts_object}
\usage{
filling_na_data_in_xts_object(xts_obj, how = "na.locf")
}
\arguments{
\item{how}{interpolation method. Options are : na.locf (last observation
carried forward), na.approx (linear interpolation), and na.spline (cubic
spline interplation )}
}
\value{
an xts object with na's interplated the "how" way.
}
\description{
takes in an xts object and attempts to interpolate missing data based on
"how" it is instructed to do so. Returns xts object with missing data
interpolated.
}

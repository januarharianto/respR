% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_funs.R
\name{adjust_scale_area}
\alias{adjust_scale_area}
\title{Convert between multipliers of the same AREA unit, e.g. mm2 to km2}
\usage{
adjust_scale_area(x, input, output)
}
\arguments{
\item{x}{numeric.}

\item{input}{string.}

\item{output}{string.}
}
\value{
A numeric.
}
\description{
This is an internal function. Converts units of area. Could be combined with
adjust_scale, but didn't know how....
}
\keyword{internal}

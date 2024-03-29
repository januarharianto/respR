% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_rate_funs.R
\name{auto_rate_highest}
\alias{auto_rate_highest}
\title{Perform rolling regression and rank from ABSOLUTE highest to lowest}
\usage{
auto_rate_highest(dt, width, by = "row")
}
\arguments{
\item{dt}{data.frame object.}

\item{width}{numeric.}

\item{by}{string.}
}
\value{
a list object with appended class \code{auto_rate_highest}
}
\description{
i.e. ignores sign. should only be used when rates are all negative or all
positive
}
\details{
This is an internal function for \code{auto_rate()}
}
\keyword{internal}

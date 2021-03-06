% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_rate.R
\name{linear_fit}
\alias{linear_fit}
\title{Perform a linear regression on a data frame}
\usage{
linear_fit(dt)
}
\arguments{
\item{dt}{data frame.}
}
\value{
A data frame object of \code{lm()} coefficients.
}
\description{
This is an internal function. Performs \code{lm} on a data frame object and returns
its coefficients.
}
\keyword{internal}

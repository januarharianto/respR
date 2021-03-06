% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_rate.R
\name{calc_rate}
\alias{calc_rate}
\title{Calculate rate of change in oxygen over time}
\usage{
calc_rate(x, from = NULL, to = NULL, by = "time", plot = TRUE)
}
\arguments{
\item{x}{data frame or object of class \code{adjust_rate}. This is the data to
process.}

\item{from}{numeric vector. Defaults to NULL. Defines the lower bound(s) of
the data frame to subset. Subsetting is based on the argument: \code{by}.}

\item{to}{numeric vector. Defaults to NULL. Defines the upper bound(s) of the
data frame to subset. Subsetting is based on the argument: \code{by}.}

\item{by}{string. \code{"time"}, \code{"row"}, \code{"o2"} or \code{"proportion"} Defaults to
\code{"time"}.This is the method used to subset the data.}

\item{plot}{logical. Defaults to TRUE. Plot the results.}
}
\value{
A list object of class \code{calc_rate}.
}
\description{
\code{calc_rate} calculates the rate of change in oxygen concentration over time
in a data frame. You can perform single or multiple regressions on subsets of
the data frame by calling the \code{from} and \code{to} arguments.
}
\details{
There are no units involved in \code{calc_rate}. This is a deliberate decision.
Units are called in a later function when volumetric and/or mass-specific
rates of oxygen use are computed in \code{\link[=convert_rate]{convert_rate()}} and \code{\link[=convert_DO]{convert_DO()}}.
}
\examples{
data("sardine.rd")
calc_rate(sardine.rd, from = 200, to = 1800)     # default subset by 'time'
calc_rate(sardine.rd, 93, 92, by = 'o2')         # subset by O2
calc_rate(sardine.rd, 1, 1000, by = 'row')     # subset by row
x <- calc_rate(sardine.rd, .8, .2, by = 'proportion') # subset by proportion
x
summary(x)
plot(x)

# Using a vector in 'from' and 'to' perform multiple measurements:
data("intermittent.rd")
calc_rate(intermittent.rd, c(200,2300,4100), c(1800,3200,4600), by = 'time')
}

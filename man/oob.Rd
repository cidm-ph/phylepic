% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_ggplot.R
\name{oob_infinite}
\alias{oob_infinite}
\title{Out of bounds handling}
\usage{
oob_infinite(x, range = c(0, 1))
}
\arguments{
\item{x}{A numeric vector of values to modify.}

\item{range}{A numeric vector of length two giving the minimum and maximum
limit of the desired output range respectively.}
}
\value{
A numerical vector of the same length as \code{x} where out of bound
values have been replaced by \code{Inf} or \code{-Inf} accordingly.
}
\description{
This helper works the same way as \code{\link[scales:oob]{scales::oob_censor()}} and similar. Out of
bounds values are pushed to positive or negative infinity. This is not useful
for builtin ggplot layers which will display a warning and drop rows with
infinite values in required aesthetics. \code{\link[=geom_calendar]{geom_calendar()}} however uses the
infinite values to indicate out of bounds values explicitly on the plot.
}

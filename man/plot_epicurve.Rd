% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_epicurve.R
\name{plot_epicurve}
\alias{plot_epicurve}
\title{Plot epidemic curve panel}
\usage{
plot_epicurve(
  phylepic,
  fill = NULL,
  weeks = TRUE,
  week_start = getOption("phylepic.week_start")
)
}
\arguments{
\item{phylepic}{Object of class "phylepic".}

\item{fill}{Variable in metadata table to use for the fill aesthetic (tidy-eval).}

\item{weeks}{When \code{TRUE}, bin the date axis by weeks.}

\item{week_start}{Day the week begins (defaults to Monday).
Can be specified as a case-insensitive English weekday name such as "Monday"
or an integer. Since you generally won't want to mix definitions, it is
more convenient to control this globally with the \code{"phylepic.week_start"}
option, e.g. \code{options(phylepic.week_start = "Monday")}.}
}
\value{
If \code{phylepic} is specified returns a ggplot, otherwise a function
that when passed a "phylepic" object produces a ggplot for use with
\code{\link[=plot.phylepic]{plot.phylepic()}}.
}
\description{
Plot epidemic curve panel
}
\seealso{
Other phylepic plots: 
\code{\link{plot.phylepic}()},
\code{\link{plot_bars}()},
\code{\link{plot_calendar}()},
\code{\link{plot_tree}()}
}
\concept{phylepic plots}

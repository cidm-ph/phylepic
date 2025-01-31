% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_epicurve.R
\name{plot_epicurve}
\alias{plot_epicurve}
\title{Plot epidemic curve panel}
\usage{
plot_epicurve(
  phylepic,
  fill = NULL,
  weeks = FALSE,
  week_start = getOption("phylepic.week_start"),
  binned = TRUE
)
}
\arguments{
\item{phylepic}{Object of class "phylepic".}

\item{fill}{Variable in metadata table to use for the fill aesthetic (tidy-eval).}

\item{weeks}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
When \code{TRUE}, bin the date axis by weeks.
Replaced by \code{binned = TRUE} paired with a suitable date scale.}

\item{week_start}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} See \code{\link[=week_breaks]{week_breaks()}}.}

\item{binned}{When \code{TRUE}, bin the date axis by the scale breaks.}
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

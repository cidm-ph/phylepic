% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_calendar.R
\docType{data}
\name{GeomCalendar}
\alias{GeomCalendar}
\alias{geom_calendar}
\title{Specialised tile geometry for calendar plots}
\usage{
geom_calendar(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  linejoin = "mitre",
  label_params = list(colour = "grey30"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping, data, stat, position, linejoin, na.rm, show.legend, inherit.aes, ...}{see \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile()}}.}

\item{label_params}{additional parameters for text labels if present
(see \code{\link[ggplot2:geom_text]{ggplot2::geom_text()}}).}
}
\description{
This geom behaves mostly the same as \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile()}} with a few
additions. Firstly, the \code{label} aesthetic is supported to draw text on top of
the tiles. Secondly, out of bounds values can be drawn as arrows at the edge
of the scale (see details below).
}
\details{
Any \code{x} values that are infinite (i.e. \code{-Inf} or \code{Inf}) would normally be
dropped by ggplot's layers. If any such values survive the stat processing,
they will be drawn by \code{geom_calendar()} as triangles at the respective edges
of the scale. This is intended to work with a scale configured to use
\code{\link[=oob_infinite]{oob_infinite()}} for out of bounds handling.
The triangles are drawn with their base (vertical edge) sitting on the scale
limit, and their width equal to half of the median bin width.

Note that the \code{label} aesthetic will be dropped if the data are not grouped
in the expected way. In general this means that all rows contributing to a
given bin must have the same value for the \code{label} aesthetic.
}
\examples{
library(ggplot2)

set.seed(1)
events <- rep(as.Date("2024-01-31") - 0:30, rpois(31, 6))
values <- round(rgamma(length(events), 1, 0.01))
df <- data.frame(date = events, value = values)

ggplot(df) +
    geom_calendar(
        aes(date, value, label = after_stat(count)),
        colour = "white",
        stat = "week_2d",
        week_start = "Monday",
        bins.y = 10
    ) +
    scale_x_week(
        limits = as.Date(c("2024-01-08", NA)),
        expand = expansion(add = 3.5)
    )
}
\keyword{datasets}

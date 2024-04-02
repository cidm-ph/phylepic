#' Date scale with breaks specified by week
#'
#' @param week_breaks,week_minor_breaks
#'   frequency of breaks in number of weeks (e.g. `2` for fortnightly breaks).
#' @param name,labels,date_labels,oob,...
#'   see [ggplot2::scale_x_date()].
#' @inheritParams week_breaks
#'
#' @return a ggplot scale object.
#' @export
scale_x_week <- function(
  name = waiver(),
  week_breaks = waiver(),
  labels = waiver(),
  date_labels = waiver(),
  week_minor_breaks = waiver(),
  oob = oob_infinite,
  ...,
  week_start = getOption("phylepic.week_start")
) {
  if (is.waive(week_breaks)) week_breaks <- 2L
  if (is.waive(week_minor_breaks)) week_minor_breaks <- 1L

  breaks <- week_breaks(week_breaks, week_start = week_start)
  minor_breaks <- week_breaks(week_minor_breaks, week_start = week_start)

  ggplot2::scale_x_date(
    name = name,
    breaks = breaks,
    date_breaks = waiver(),
    labels = labels,
    date_labels = date_labels,
    minor_breaks = minor_breaks,
    date_minor_breaks = waiver(),
    oob = oob,
    ...
  )
}

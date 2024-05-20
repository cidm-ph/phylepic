#' Date scale with breaks specified by week
#'
#' This produces a scale that is measured in days as with [ggplot2::scale_x_date],
#' however it will snap breaks and limits to week boundaries so that things
#' work as intended when binning by week.
#'
#' Any `limits` specified are converted to the nearest week boundary that
#' includes the specified dates, i.e. the lower limit will be rounded down and
#' the upper limit rounded up so that the limits are week boundaries.
#'
#' @param week_breaks,week_minor_breaks
#'   frequency of breaks in number of weeks (e.g. `2` for fortnightly breaks).
#' @param name,labels,date_labels,oob,limits,...
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
  limits = NULL,
  ...,
  week_start = getOption("phylepic.week_start")
) {
  if (is.waive(week_breaks)) week_breaks <- 2L
  if (is.waive(week_minor_breaks)) week_minor_breaks <- 1L

  breaks <- week_breaks(week_breaks, week_start = week_start)
  minor_breaks <- week_breaks(week_minor_breaks, week_start = week_start)
  if (!is.null(limits)) {
    lim_lo <- if (is.na(limits[[1]])) NA else floor_week(limits[[1]])
    lim_hi_idx <- if (length(limits > 1)) 2L else 1L
    lim_hi <- if (is.na(limits[[lim_hi_idx]])) NA else ceiling_week(limits[[lim_hi_idx]])
    limits <- date_to_week(c(lim_lo, lim_hi), as_week_start(week_start))
  }

  ggplot2::scale_x_date(
    name = name,
    breaks = breaks,
    date_breaks = waiver(),
    labels = labels,
    date_labels = date_labels,
    minor_breaks = minor_breaks,
    date_minor_breaks = waiver(),
    oob = oob,
    limits = limits,
    ...
  )
}

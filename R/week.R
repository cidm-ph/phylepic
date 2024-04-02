.weekdays <- c(
  monday = 5L,
  tuesday = 6L,
  wednesday = 0L,
  thursday = 1L,
  friday = 2L,
  saturday = 3L,
  sunday = 4L
)

as_week_start <- function(week_start) {
  if (is.null(week_start)) {
    5L
  } else if (length(week_start) > 1L) {
    cli::cli_abort("{.arg week_start} must be a single value, not a vector.")
  } else if (is.numeric(week_start)) {
    as.integer(week_start %% 7L)
  } else if (is.character(week_start) && (tolower(week_start) %in% names(.weekdays))) {
    unname(.weekdays[tolower(week_start)])
  } else {
    cli::cli_abort(c(
      "!" = "{.val {week_start}} is not a valid {.arg week_start}.",
      "i" = "{.arg week_start} should be an English weekday name like {.val {'monday'}} or an integer."
    ))
  }
}

#' Breaks for week-binning date axes
#'
#' @param width Number of weeks between breaks (e.g. `2` will give a break every
#'   fortnight).
#' @param week_start Day the week begins (defaults to Monday).
#'   Can be specified as a case-insensitive English weekday name such as "Monday"
#'   or an integer. Since you generally won't want to mix definitions, it is
#'   more convenient to control this globally with the `"phylepic.week_start"`
#'   option, e.g. `options(phylepic.week_start = "Monday")`.
#'
#' @return A break function suitable for use in [ggplot2::scale_x_date()] et al.
#' @export
week_breaks <- function(
  width = 1L,
  week_start = getOption("phylepic.week_start")
) {
  week_start <- as_week_start(week_start)
  function(x) week_span(x, width, week_start)
}

week_span <- function(x_range, width, week_start) {
  x_range <- date_to_week(
    c(floor_week(x_range[1]), ceiling_week(x_range[2])),
    week_start
  )
  seq(x_range[1], x_range[2], by = paste0(width, " week"))
}

floor_week <- function(date) {
  as.Date(cut(date, "1 week", right = TRUE, include.lowest = TRUE))
}

ceiling_week <- function(date, time) {
  floor_week(date + 7L)
}

date_to_week <- function(x, week_start) {
  if (!inherits(x, "Date")) {
    cli::cli_abort("{.fn stat_week} works with objects of class {.cls Date} only")
  }

  wday <- as.POSIXlt(x)$wday + 1L
  wday <- 1L + (wday + (6L - week_start)) %% 7L

  x + (4L - wday)
}

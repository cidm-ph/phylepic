#' Calculate week bins with additional binning in the y axis
#'
#' Computes week bins for date data in the x aesthetic, and allows
#' the binning to be specified for the y aesthetic. This is mostly equivalent to
#' [ggplot2::stat_bin_2d()] with the x aesthetic handling fixed to weeks.
#'
#' The computed aesthetics are similar to those of `stat_bin_2d()`, including
#' `after_stat(count)`, `after_stat(density)`, and the bin positions and sizes:
#' `after_stat(xmin)`, `after_stat(height)`, and so on.
#'
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,...
#'   See [ggplot2::stat_bin_2d].
#' @param bins.y,binwidth.y,breaks.y,center.y,boundary.y,closed.y
#'   See the analogous parameters in [ggplot2::stat_bin_2d].
#' @param drop drop bins with zero count.
#' @inheritParams week_breaks
#'
#' @return ggplot2 stat layer.
#' @export
#' @examples
#' library(ggplot2)
#'
#' set.seed(1)
#' events <- rep(as.Date("2024-01-31") - 0:30, rpois(31, 6))
#' values <- round(rgamma(length(events), 1, 0.01))
#' df <- data.frame(date = events, value = values)
#'
#' ggplot(df) + stat_week_2d(aes(date, value), week_start = "Monday")
stat_week_2d <- function(
  mapping = NULL,
  data = NULL,
  geom = "tile",
  position = "identity",
  ...,
  bins.y = NULL,
  binwidth.y = NULL,
  breaks.y = NULL,
  center.y = NULL,
  boundary.y = NULL,
  closed.y = c("left", "right"),
  drop = TRUE,
  week_start = getOption("phylepic.week_start"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatWeek2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      drop = drop,
      na.rm = na.rm,
      week_start = week_start,
      bins.y = bins.y,
      binwidth.y = binwidth.y,
      breaks.y = breaks.y,
      center.y = center.y,
      boundary.y = boundary.y,
      closed.y = closed.y,
      ...
    )
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_week
StatWeek2d <- ggplot2::ggproto("StatWeek2d", ggplot2::Stat,
  default_aes = aes(weight = 1, fill = ggplot2::after_stat(count)),

  required_aes = c("x", "y"),
  dropped_aes = c("weight", "inf"),

  setup_params = function(self, data, params) {
    params$week_start <- as_week_start(params$week_start)

    if (!is.null(params$boundary.y) && !is.null(params$center.y)) {
      cli::cli_abort("Only one of {.arg boundary.y} and {.arg center.y} may be specified in {.fn stat_week_2d}.")
    }

    if (is.null(params$breaks.y) && is.null(params$binwidth.y) && is.null(params$bins.y)) {
      cli::cli_inform("{.fn stat_week_2d} using {.code bins.y = 30}. Pick better value with {.arg binwidth.y}.")
      params$bins.y <- 30
    }

    params
  },

  setup_data = function(self, data, params) {
    # infinite values in requires aes will lead to dropped rows, so mark and mask
    data$inf <- rep(0, nrow(data))
    data$inf[is.infinite(data$x)] <- data$x[is.infinite(data$x)]
    data$x[is.infinite(data$x)] <- 0
    data
  },

  compute_group = function(
    data, scales,
    binwidth.y = NULL, bins.y = 30, breaks.y = NULL, center.y = NULL,
    boundary.y = NULL, closed.y = c("left", "right"), drop = TRUE, week_start = 5L
  ) {
    if (!inherits(scales$x, "ScaleContinuousDate")) {
      cli::cli_abort(c(
        "{.fn stat_week_2d} only works with date {.val x} scales",
        "x" = "Scale for aesthetic {.val x} is {.cls {class(scales$x)}}"
      ))
    }

    x_range <- scales$x$get_limits()
    x_range <- scales$x$trans$inverse(x_range)
    x_breaks <- week_span(x_range, 1L, week_start)

    x_bins <- make_bins(data$x, scales$x, breaks = x_breaks, closed = "right")
    y_bins <- make_bins(
      data$y, scales$y, breaks = breaks.y, binwidth = binwidth.y, bins = bins.y,
      center = center.y, boundary = boundary.y, closed = closed.y
    )

    binned_data <- bin_vector2d(
      x = ifelse(data$inf == 0, data$x, data$inf),
      y = data$y,
      bins.x = x_bins,
      bins.y = y_bins,
      weight = data$weight,
      drop = drop
    )

    if (!any(duplicated(binned_data$y)) & all(binned_data$y %in% data$y)) {
      idx <- match(binned_data$y, data$y)
      binned_data$xorig <- scales::date_trans()$inverse(data$x[idx])
    }

    binned_data
  },
)

bin_vector2d <- function(x, y, bins.x, bins.y, weight = NULL, drop = TRUE) {
  stopifnot(is_bins(bins.x), is_bins(bins.y))

  if (all(is.na(x)) || all(is.na(y))) {
    return(bin2d_out(
      length(x),
      x = NA, width = NA, xmin = NA, xmax = NA,
      y = NA, height = NA, ymin = NA, ymax = NA
    ))
  }

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }

  x_breaks <- c(-Inf, bins.x$fuzzy, Inf)
  y_breaks <- bins.y$fuzzy
  x_bin_idx <- cut(x, x_breaks, right = bins.x$right_closed, include.lowest = TRUE, labels = FALSE)
  y_bin_idx <- cut(y, y_breaks, right = bins.y$right_closed, include.lowest = TRUE, labels = FALSE)

  # One side of Inf is missed from the binning since the intervals are half open:
  if (bins.x$right_closed) {
    x_bin_idx[is.na(x_bin_idx) & !is.na(x)] <- 1L
  } else {
    x_bin_idx[is.na(x_bin_idx) & !is.na(x)] <- nlevels(x_bin_idx)
  }

  index <- list(xbin = x_bin_idx, ybin = y_bin_idx)

  labels <- lapply(index, ulevels)
  out <- expand.grid(labels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  grps <- split(weight, index)
  names(grps) <- NULL
  out$count <- unlist(lapply(grps, sum))
  out$count[is.na(out$count)] <- 0

  if (drop) {
    n <- lengths(grps)
    out <- out[n > 0, , drop = FALSE]
  } else {
    all_labels <- list(xbin = seq_along(bins.x$fuzzy), ybin = seq_along(bins.y$fuzzy))
    out_full <- expand.grid(all_labels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    out <- dplyr::left_join(out_full, out, by = c("xbin", "ybin"))
    out$count[is.na(out$count)] <- 0
  }

  bin_x <- (bins.x$breaks[-length(bins.x$breaks)] + bins.x$breaks[-1]) / 2
  bin_y <- (bins.y$breaks[-length(bins.y$breaks)] + bins.y$breaks[-1]) / 2
  bin_widths <- diff(bins.x$breaks)
  bin_heights <- diff(bins.y$breaks)

  bin_x <- c(-Inf, bin_x, Inf)
  bin_widths <- c(0, bin_widths, 0)

  # Add row for missings
  if (any(is.na(bins.x))) {
    stop("missing bins.x entries are not yet implemented for stat_week_2d")
    # bin_widths <- c(bin_widths, NA)
    # bin_x <- c(bin_x, NA)
  }
  if (any(is.na(bins.y))) {
    stop("missing bins.y entries are not yet implemented for stat_week_2d")
  }

  bin2d_out(
    count = out$count,
    x = bin_x[out$xbin], width = bin_widths[out$xbin],
    y = bin_y[out$ybin], height = bin_heights[out$ybin]
  )
}

bin2d_out <- function(
  count = integer(0),
  x = numeric(0),  width = numeric(0),
  y = numeric(0), height = numeric(0),
  xmin = x -  width / 2, xmax = x +  width / 2,
  ymin = y - height / 2, ymax = y + height / 2
) {
  density <- count / sum(abs(count))

  vctrs::data_frame(
    count = count,
    x = x,
    xmin = xmin,
    xmax = xmax,
    width = width,
    y = y,
    ymin = ymin,
    ymax = ymax,
    height = height,
    density = density,
    ncount = count / max(abs(count)),
    ndensity = density / max(abs(density)),
    .size = length(count),
    .name_repair = "minimal"
  )
}

make_bins <- function(
  x, scale, breaks = NULL, binwidth = NULL, bins = NULL, center = NULL,
  boundary = NULL, closed = c("right", "left")
) {
  if (!is.null(breaks)) {
    if (!scale$is_discrete()) {
      breaks <- scale$transform(breaks)
    }
    bin_breaks(breaks, closed)
  } else if (!is.null(binwidth)) {
    if (is.function(binwidth)) {
      binwidth <- binwidth(x)
    }
    bin_breaks_width(
      scale$dimension(),
      binwidth,
      center = center,
      boundary = boundary,
      closed = closed
    )
  } else {
    bin_breaks_bins(
      scale$dimension(),
      bins,
      center = center,
      boundary = boundary,
      closed = closed
    )
  }
}

ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else if (is.null(x)) {
    x
  } else {
    sort(vctrs::vec_unique(x))
  }
}

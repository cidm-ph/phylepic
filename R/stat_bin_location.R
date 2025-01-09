#' Annotate rows with 2D bin positions
#'
#' Unlike normal binning stat, this stat does not change the number of rows in
#' the data. Rather than summing weights, it merely adds `xmin`, `xmax`, `ymin`,
#' and `ymax` values to the original data. This is useful for [geom_calendar()],
#' which only has one tile per row and therefore would only have a single entry
#' contributing to each bin. [ggplot2::stat_bin_2d()] would cause the other
#' fields to be discarded since it summarises the data.
#'
#' @inheritParams stat_bin_2d_auto
#' @param overflow If `TRUE`, map values that would normally be outside the
#'   range to peripheral bins that span from the closest limit to the closest
#'   infinity. You can control this for x and y separately by passing a list.
#'
#' @return ggplot2 stat layer.
#' @export
stat_bin_location <- function(
    mapping = NULL,
    data = NULL,
    geom = "rect",
    position = "identity",
    ...,
    overflow = FALSE,
    breaks = NULL,
    bins = 30,
    binwidth = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBinLocation,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      overflow = overflow,
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_bin_location
StatBinLocation <- ggplot2::ggproto("StatBinLocation", ggplot2::Stat,
  required_aes = c("x", "y"),

  setup_params = function(self, data, params) {
    vars <- c("origin", "binwidth", "breaks", "center", "boundary")
    params[vars] <- lapply(params[vars], dual_param, default = NULL)
    params$closed <- dual_param(params$closed, list(x = "right", y = "right"))
    params$overflow <- dual_param(params$overflow, list(x = FALSE, y = FALSE))
    params$bins <- dual_param(params$bins, list(x = 30, y = 30))

    params
  },

  compute_group = function(
    data, scales,
    overflow = list(x = TRUE, y = TRUE),
    binwidth = NULL,
    bins = list(x = 30, y = 30),
    breaks = NULL, origin = NULL, boundary = 0, closed = NULL, center = NULL
  ) {
    if (!is.null(breaks$x) && is.character(breaks$x)) {
      breaks$x <- breaks_from_scale(breaks$x, scales$x)
    }
    if (!is.null(breaks$y) && is.character(breaks$y)) {
      breaks$y <- breaks_from_scale(breaks$y, scales$y)
    }

    xbin <- compute_bins(
      data$x, scales$x, breaks$x, binwidth$x, bins$x,
      center$x, boundary$x, closed$x
    )
    ybin <- compute_bins(
      data$y, scales$y, breaks$y, binwidth$y, bins$y,
      center$y, boundary$y, closed$y
    )
    cut_id <- list(
      xbin = as.integer(bin_cut_with_overflow(data$x, xbin, overflow$x)),
      ybin = as.integer(bin_cut_with_overflow(data$y, ybin, overflow$y))
    )

    xdim <- bin_loc_with_overflow(xbin$breaks, cut_id$xbin, overflow$x)
    data$xmin <- xdim$left
    data$xmax <- xdim$right

    ydim <- bin_loc_with_overflow(ybin$breaks, cut_id$ybin, overflow$y)
    data$ymin <- ydim$left
    data$ymax <- ydim$right

    data
  }
)

bin_cut_with_overflow <- function(x, bins, overflow = TRUE) {
  if (!overflow) return(bin_cut(x, bins))

  breaks <- c(-Inf, bins$fuzzy, Inf)
  bin_idx <- cut(x, breaks, right = bins$right_closed, include.lowest = TRUE, labels = FALSE)

  # One side of Inf is missed from the binning since the intervals are half open:
  if (bins$right_closed) {
    bin_idx[is.na(bin_idx) & !is.na(x)] <- 1L
  } else {
    bin_idx[is.na(bin_idx) & !is.na(x)] <- nlevels(bin_idx)
  }

  bin_idx
}

bin_loc_with_overflow <- function(x, id, overflow = TRUE) {
  if (!overflow) return(bin_loc(x, id))

  x <- c(-Inf, x, Inf)
  left <- x[-length(x)]
  right <- x[-1]

  res <- list(
    left = left[id],
    right = right[id],
    mid = ((left + right) / 2)[id],
    length = diff(x)[id]
  )
  res$length[is.infinite(res$length)] <- 0
  res
}

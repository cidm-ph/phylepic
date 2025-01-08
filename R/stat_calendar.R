#' @export
#' @rdname stat_bin_2d_auto
stat_calendar <- function(
    mapping = NULL,
    data = NULL,
    geom = "tile",
    position = "identity",
    ...,
    breaks = "all",
    bins = 30,
    binwidth = NULL,
    drop = TRUE,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatCalendar,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_bin_2d_auto
StatCalendar <- ggplot2::ggproto("StatCalendar", ggplot2::StatBin2d,
  dropped_aes = c("weight", "inf"),

  setup_params = function(self, data, params) {
    if (is.character(params$drop)) {
      params$drop <- !identical(params$drop, "none")
    }

    # params <- fix_bin_params(params, fun = snake_class(self), version = "3.5.2")
    vars <- c("origin", "binwidth", "breaks", "center", "boundary")
    params[vars] <- lapply(params[vars], dual_param, default = NULL)
    params$closed <- dual_param(params$closed, list(x = "right", y = "right"))

    params
  },

  compute_group = function(
    data, scales,
    binwidth = NULL, bins = 30, breaks = NULL, origin = NULL, drop = TRUE,
    boundary = 0, closed = NULL, center = NULL
  ) {
    if (!inherits(scales$x, "ScaleContinuousDate")) {
      cli::cli_abort(c(
        "{.fn stat_calendar} only works with date {.val x} scales",
        "x" = "Scale for aesthetic {.val x} is {.cls {class(scales$x)}}"
      ))
    }

    bins <- dual_param(bins, list(x = 30, y = 30))
    if (!is.null(breaks$x) && is.character(breaks$x)) {
      breaks$x <- breaks_from_scale(breaks$x, scales$x)
    }
    if (!is.null(breaks$y) && is.character(breaks$y)) {
      breaks$y <- breaks_from_scale(breaks$y, scales$y)
    }

    if (is.null(data$weight)) data$weight <- 1

    xbin <- compute_bins(
      data$x, scales$x, breaks$x, binwidth$x, bins$x,
      center$x, boundary$x, closed$x
    )
    ybin <- compute_bins(
      data$y, scales$y, breaks$y, binwidth$y, bins$y,
      center$y, boundary$y, closed$y
    )
    cut_id <- list(
      xbin = as.integer(bin_cut_with_overflow(data$x, xbin)),
      ybin = as.integer(bin_cut(data$y, ybin))
    )

    out <- tapply_df(data$weight, cut_id, sum, drop = drop)

    xdim <- bin_loc_with_overflow(xbin$breaks, out$xbin)
    out$x <- xdim$mid
    out$width <- xdim$length

    ydim <- bin_loc(ybin$breaks, out$ybin)
    out$y <- ydim$mid
    out$height <- ydim$length

    if (!any(duplicated(out$y)) & all(out$y %in% data$y)) {
      idx <- match(out$y, data$y)
      out$xorig <- scales::date_trans()$inverse(data$x[idx])
    }

    out
  }
)

bin_cut_with_overflow <- function(x, bins) {
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

bin_loc_with_overflow <- function(x, id) {
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

#' Calculate two-dimensional bins using scale breaks
#'
#' This is mostly equivalent to [ggplot2::stat_bin_2d()] except that the bin
#' edges can be copied from the scale breaks. For this effect to work properly,
#' you either need to use fixed scale breaks (e.g. using a vector instead of a
#' function), or use the [cached_breaks()] helper.
#'
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,pad,...
#'   See [ggplot2::stat_bin()].
#' @param breaks Controls the break positions for the bins.
#'   Can be `NULL`, a numeric vector, or a function as per [ggplot2::stat_bin()].
#'   Can additionally be a character specifying which breaks from the scale
#'   should be used: `"minor"` for minor breaks, `"major"` for major breaks,
#'   or `"all"` for both.
#'   This can be a scalar or a list of length 2 to control the axes separately.
#'
#' @return ggplot2 stat layer.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds, aes(x, y)) +
#'   scale_x_continuous(limits = c(4, 10)) +
#'   scale_y_continuous(limits = c(4, 10)) +
#'   stat_bin_2d_auto()
#'
#' # You can control the x and y binning separately:
#' ggplot(diamonds, aes(x, y)) +
#'   scale_x_continuous(limits = c(4, 10)) +
#'   scale_y_continuous(limits = c(4, 10)) +
#'   stat_bin_2d_auto(breaks = list("major", NULL), bins = list(NULL, 20))
stat_bin_2d_auto <- function(
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
    stat = StatBin2dAuto,
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
stat_bin2d_auto <- stat_bin_2d_auto

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_bin_2d_auto
StatBin2dAuto <- ggplot2::ggproto("StatBin2dAuto", ggplot2::StatBin2d,
  compute_group = function(
    data, scales,
    binwidth = NULL, bins = 30, breaks = "all", origin = NULL, drop = TRUE
  ) {
    breaks <- dual_param(breaks, list(x = "all", y = "all"))
    if (!is.null(breaks$x) && is.character(breaks$x)) {
      breaks$x <- breaks_from_scale(breaks$x, scales$x)
    }
    if (!is.null(breaks$y) && is.character(breaks$y)) {
      breaks$y <- breaks_from_scale(breaks$y, scales$y)
    }

    ggplot2::StatBin2d$compute_group(
      data, scales,
      binwidth = binwidth,
      bins = bins,
      breaks = breaks,
      origin = origin,
      drop = drop
    )
  }
)

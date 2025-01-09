#' Calculate bins using scale breaks
#'
#' This is mostly equivalent to [ggplot2::stat_bin()] except that the bin edges
#' are copied from the scale breaks. For this effect to work properly, you
#' either need to use fixed scale breaks (e.g. using a vector instead of a
#' function), or use the [breaks_cached()] helper.
#'
#' @param breaks Which breaks from the scale should be used? `"minor"` for minor
#'   breaks, `"major"` for major breaks, or `"all"` for both.
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,pad,...
#'   See [ggplot2::stat_bin()].
#' @param bins,binwidth,centre,boundary
#'   Ignored.
#'
#' @return ggplot2 stat layer.
#' @export
#' @examples
#' library(ggplot2)
#'
#' set.seed(1)
#' events <- rep(as.Date("2024-01-31") - 0:30, rpois(31, 2))
#' df <- data.frame(date = events)
#'
#' ggplot(df) +
#'   stat_bin_auto(aes(date)) +
#'   scale_x_date(breaks = week_breaks(2L, week_start = "Monday"))
stat_bin_auto <- function(
  mapping = NULL,
  data = NULL,
  geom = "bar",
  position = "stack",
  ...,
  breaks = "all",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  pad = FALSE,
  binwidth = NULL,
  bins = NULL,
  centre = NULL,
  boundary = NULL
) {
  ggplot2::layer(
    stat = StatBinAuto,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      pad = pad,
      ...
    )
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_bin_auto
StatBinAuto <- ggplot2::ggproto("StatBinAuto", ggplot2::StatBin,
  setup_params = function(self, data, params) {
    breaks_choices <- c("minor", "major", "all")
    if (!is.null(params$breaks) && !params$breaks %in% breaks_choices) {
      cli::cli_abort("{.arg breaks} must be in {breaks_choices}, not {params$breaks}")
    }

    # suppress warning about default bins (this is unused anyway)
    params$bins <- 1

    ggplot2::ggproto_parent(ggplot2::StatBin, self)$setup_params(data, params)
  },

  compute_group = function(
    data, scales, flipped_aes = FALSE, week_start = 5L, pad = FALSE,
    closed = "right", breaks = "all", ...,
    # ignored
    binwidth = NULL, bins = NULL, boundary = NULL, centre = NULL
  ) {
    x <- ggplot2::flipped_names(flipped_aes)$x
    breaks <- breaks_from_scale(breaks, scales[[x]])

    ggplot2::StatBin$compute_group(
      data, scales, breaks = breaks, flipped_aes = flipped_aes, pad = pad,
      closed = closed, ...
    )
  }
)

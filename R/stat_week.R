#' Calculate week bins from dates
#'
#' Computes weeks for date data. This is mostly equivalent to
#' [ggplot2::stat_bin()] with the bins fixed to weeks starting on a particular
#' day.
#'
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,pad,...
#'   See [ggplot2::stat_bin()].
#' @inheritParams week_breaks
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
#' ggplot(df) + stat_week(aes(date), week_start = "Monday")
#'
#' # or equivalently:
#' # ggplot(df) + geom_bar(aes(date), stat = "week", week_start = "Monday")
stat_week <- function(
  mapping = NULL,
  data = NULL,
  geom = "bar",
  position = "stack",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  week_start = getOption("phylepic.week_start"),
  pad = FALSE
) {
  ggplot2::layer(
    stat = StatWeek,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      week_start = week_start,
      pad = pad,
      ...
    )
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname stat_week
StatWeek <- ggplot2::ggproto("StatWeek", ggplot2::StatBin,
  setup_params = function(self, data, params) {
    params$week_start <- as_week_start(params$week_start)
    params$closed <- "right"
    params$breaks <- ggplot2::waiver()

    ggplot2::ggproto_parent(ggplot2::StatBin, self)$setup_params(data, params)
  },

  compute_group = function(
    data, scales, flipped_aes = FALSE, week_start = 5L, breaks = NULL,
    pad = FALSE, closed = "right", ...
  ) {
    x <- ggplot2::flipped_names(flipped_aes)$x
    if (!inherits(scales[[x]], "ScaleContinuousDate")) {
      cli::cli_abort(c(
        "{.fn stat_week} only works with date scales",
        "x" = "Scale for aesthetic {.val {x}} is {.cls {class(scales[[x]])}}"
      ))
    }

    x_range <- scales[[x]]$get_limits()
    x_range <- scales[[x]]$trans$inverse(x_range)
    breaks <- week_span(x_range, 1L, week_start)

    ggplot2::StatBin$compute_group(
      data, scales, breaks = breaks, flipped_aes = flipped_aes, pad = pad,
      closed = closed, ...
    )
  }
)

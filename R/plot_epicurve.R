#' Plot epidemic curve panel
#'
#' @param phylepic Object of class "phylepic".
#' @param fill Variable in metadata table to use for the fill aesthetic (tidy-eval).
#' @param weeks When `TRUE`, bin the date axis by weeks.
#' @inheritParams week_breaks
#'
#' @inherit plot_tree return
#' @family phylepic plots
#' @export
plot_epicurve <- function(
  phylepic,
  fill = NULL,
  weeks = TRUE,
  week_start = getOption("phylepic.week_start")
) {
  wrapper <- function(x) {
    x <- as.data.frame(x)
    stopifnot(is.tip_data(x))

    p <- ggplot2::ggplot(x)

    if (weeks) {
      p <- p + stat_week(
        aes(x = .data$.phylepic.date, fill = {{fill}}),
        week_start = week_start
      )
    } else {
      p <- p + ggplot2::geom_histogram(
        aes(x = .data$.phylepic.date, fill = {{fill}}),
        binwidth = 1
      )
    }

    p +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(position = "right") +
      ggplot2::labs(x = NULL, y = NULL) +
      theme_plot_epicurve()
  }

  plot <- if (missing(phylepic)) wrapper else wrapper(phylepic)
  annotate_conditions_with_panel(plot, "epicurve")
}

theme_plot_epicurve <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_line(),
    plot.margin = ggplot2::margin(l = 0, r = 0, t = 5, unit = "pt"),
    panel.grid.major.x = ggplot2::element_line(colour = "#aaaaaa"),
  )
}

conform_plot_epicurve <- function(plot, scale.date, scale.fill) {
  if (!is.null(scale.date)) {
    plot <- replace_scale(plot, scale.date)
    plot <- mutate_scale(plot, "x", f = function(scale) {
      scale$position <- "bottom"
      scale
    })
  }

  if (!is.null(scale.fill)) {
    plot <- plot + scale.fill
  }

  if (! inherits(plot$scales$get_scales("x"), "ScaleContinuousDate")) {
    cli::cli_abort(c(
      "x" = "{.arg plot.calendar} does not have a date scale for {.field x}"
    ))
  }

  plot <- mutate_scale(plot, "y", ggplot2::scale_y_continuous(), f = function(scale) {
    # the documented default is 5% expansion, so replace now
    if (is.waive(scale$expand)) scale$expand <- ggplot2::expansion(mult = 0.05)

    # don't expand the lower limit
    scale$expand[[1]] <- 0
    scale$expand[[2]] <- 0
    scale
  })

  plot <- plot + ggplot2::theme(legend.position = "none")
  annotate_conditions_with_panel(plot, "epicurve")
}

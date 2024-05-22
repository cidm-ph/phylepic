#' Plot calendar panel
#'
#' @param phylepic Object of class "phylepic".
#' @param fill Variable in metadata table to use for the fill aesthetic (tidy-eval).
#' @param weeks When `TRUE`, bin the date axis by weeks.
#' @inheritParams week_breaks
#' @param labels Controls the format of date labels on calendar tiles.
#'   If `NULL`, no labels are drawn.
#'   If a character scalar, controls the date format (see [`strptime()`]).
#' @param labels.params Passed to [`ggplot2::geom_text()`] if `labels` are drawn.
#'
#' @inherit plot_tree return
#' @family phylepic plots
#' @export
plot_calendar <- function(
  phylepic,
  fill = NULL,
  weeks = TRUE,
  week_start = getOption("phylepic.week_start"),
  labels = NULL,
  labels.params = list(size = 3, fontface = "bold", colour = "white")
) {
  wrapper <- function(x) {
    x <- as.data.frame(x)
    stopifnot(is.tip_data(x))

    mapping <- aes(fill = {{fill}})

    p <- ggplot2::ggplot(x, aes(
      y = .data$.phylepic.index,
      x = .data$.phylepic.date
    ))

    if (weeks) {
      if (!is.null(labels)) {
        mapping2 <- aes(label = format(ggplot2::after_stat(.data$xorig), labels))
        mapping$label <- mapping2$label
      }

      p <- p + stat_week_2d(
        mapping = mapping,
        week_start = week_start,
        binwidth.y = 1L,
        na.rm = TRUE,
        geom = "calendar",
        linewidth = 0.3,
        label_params = labels.params
      )
    } else {
      if (!is.null(labels)) {
        mapping2 <- aes(label = format(ggplot2::after_stat(x), labels))
        mapping$label <- mapping2$label
      }

      p <- p + geom_calendar(
        mapping,
        width = 1L,
        height = 1L,
        na.rm = TRUE,
        linewidth = 0.3,
        label_params = labels.params
      )
    }

    p +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(add = 0),
        limits = c(-0.5, nrow(x) - 0.5)
      ) +
      theme_plot_calendar() +
      coord_tree()
  }

  plot <- if (missing(phylepic)) wrapper else wrapper(phylepic)
  annotate_conditions_with_panel(plot, "calendar")
}

#' @importFrom ggplot2 rel
theme_plot_calendar <- function() {
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = "grey70"),
    panel.grid.minor.x = ggplot2::element_line(linewidth = rel(0.5)),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(fill = NA),
    plot.margin = ggplot2::margin(l = 0, r = 0),
  )
}

conform_plot_calendar <- function(plot, scale.date, scale.fill, n) {
  # The y scale expansion must match or else the plots won't line up.
  plot <- mutate_scale(plot, "y", ggplot2::scale_y_continuous(), f = function(scale) {
    scale$expand <- ggplot2::expansion(add = 0)
    scale$limits <- c(-0.5, n - 0.5)
    scale
  })

  if (!is.null(scale.date)) {
    plot <- replace_scale(plot, scale.date)
    plot <- mutate_scale(plot, "x", f = function(scale) {
      scale$position <- "top"
      scale
    })
  }

  if (!is.null(scale.fill)) {
    plot <- plot + scale.fill
  }

  if (! inherits(plot$scales$get_scales("x"), "ScaleContinuousDate")) {
    cli::cli_abort(c("x" = "{.arg plot.calendar} does not have a date scale for {.field x}"))
  }

  plot <- plot + ggplot2::theme(legend.position = "none")
  annotate_conditions_with_panel(plot, "calendar")
}

#' Plot calendar panel
#'
#' @param phylepic Object of class "phylepic".
#' @param fill Variable in metadata table to use for the fill aesthetic (tidy-eval).
#' @param weeks `r lifecycle::badge("deprecated")`
#'   When `TRUE`, bin the date axis by weeks.
#'   Replaced by `binned = TRUE` paired with a suitable date scale.
#' @param week_start `r lifecycle::badge("deprecated")` See [week_breaks()].
#' @param binned When `TRUE`, bin the date axis by the scale breaks.
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
  weeks = FALSE,
  week_start = getOption("phylepic.week_start"),
  binned = TRUE,
  labels = NULL,
  labels.params = list(size = 3, fontface = "bold", colour = "white")
) {
  wrapper <- function(x) {
    x <- as.data.frame(x)
    stopifnot(is.tip_data(x))

    mapping <- aes(fill = {{fill}})

    main_layer <- if (weeks) {
      lifecycle::deprecate_warn(
        "0.3.0", "plot_epicurve(weeks)",
        details = "Use plot_epicurve(binned = TRUE) and adjust breaks on the date scale"
      )

      if (!is.null(labels)) {
        mapping2 <- aes(label = format(ggplot2::after_stat(.data$xorig), labels))
        mapping$label <- mapping2$label
      }

      stat_week_2d(
        mapping = mapping,
        week_start = week_start,
        binwidth.y = 1L,
        na.rm = TRUE,
        geom = "calendar",
        linewidth = 0.3,
        label_params = labels.params
      )
    } else if (binned) {
      if (!is.null(labels)) {
        mapping2 <- aes(label = ggplot2::after_stat(format(structure(x, class = "Date"), labels)))
        mapping$label <- mapping2$label
      }

      geom_calendar(
        mapping,
        na.rm = TRUE,
        linewidth = 0.3,
        label_params = labels.params,
        overflow = list(x = TRUE, y = FALSE),
        breaks = list(x = "all", y = NULL),
        binwidth = list(x = NULL, y = 1L)
      )
    } else {
      if (!is.null(labels)) {
        mapping2 <- aes(
          label = format(ggplot2::after_stat(x), labels),
        )
        mapping$label <- mapping2$label
      }

      geom_calendar(
        mapping,
        stat = "identity",
        binwidth = 1L,
        na.rm = TRUE,
        linewidth = 0.3,
        label_params = labels.params
      )
    }

    ggplot2::ggplot(x, aes(
      y = .data$.phylepic.index,
      x = .data$.phylepic.date
    )) +
      main_layer +
      ggplot2::scale_x_date(breaks =  breaks_cached(scales::breaks_extended())) +
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

conform_plot_calendar <- function(plot, n) {
  plot <- patch_scale(
    plot, "x", ggplot2::scale_x_date, list(
    position = "bottom"
  ), panel_name = "calendar", call = rlang::caller_call())

  plot <- patch_scale(
    plot, "y", ggplot2::scale_y_continuous, list(
      # the documented default is 5% expansion, so replace now
      expand = ggplot2::expansion(0),
      limits = c(-0.5, n - 0.5)
    ), panel_name = "calendar", call = rlang::caller_call()
  )

  plot <- plot + ggplot2::theme(legend.position = "none")
  annotate_conditions_with_panel(plot, "calendar")
}

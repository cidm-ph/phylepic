#' Plot epidemic curve panel
#'
#' @param phylepic Object of class "phylepic".
#' @param fill Variable in metadata table to use for the fill aesthetic (tidy-eval).
#' @param weeks `r lifecycle::badge("deprecated")`
#'   When `TRUE`, bin the date axis by weeks.
#'   Replaced by `binned = TRUE` paired with a suitable date scale.
#' @param week_start `r lifecycle::badge("deprecated")` See [week_breaks()].
#' @param binned When `TRUE`, bin the date axis by the scale breaks.
#'
#' @inherit plot_tree return
#' @family phylepic plots
#' @export
plot_epicurve <- function(
  phylepic,
  fill = NULL,
  weeks = FALSE,
  week_start = getOption("phylepic.week_start"),
  binned = TRUE
) {
  wrapper <- function(x) {
    x <- as.data.frame(x)
    stopifnot(is.tip_data(x))

    main_layer <- if (weeks) {
      lifecycle::deprecate_warn(
        "0.3.0", "plot_epicurve(weeks)",
        details = "Use plot_epicurve(binned = TRUE) and adjust breaks on the date scale"
      )
      stat_week(
        aes(x = .data$.phylepic.date, fill = {{fill}}),
        week_start = week_start
      )
    } else {
      hist_args <-
        if (binned) list(stat = "bin_auto", breaks = "all")
        else list(binwidth = 1)
      rlang::inject(
        ggplot2::geom_histogram(
          aes(x = .data$.phylepic.date, fill = {{fill}}),
          colour = "black",
          linewidth = 0.2,
          !!!hist_args
        )
      )
    }
    ggplot2::ggplot(x) +
      main_layer +
      ggplot2::scale_x_date(breaks =  breaks_cached(scales::breaks_extended())) +
      ggplot2::scale_y_continuous(
        position = "right",
        expand = ggplot2::expansion(0),
        breaks = scales::breaks_extended(Q = c(1, 5, 2, 4, 3))
      ) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal() +
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

conform_plot_epicurve <- function(plot, date_limits = NULL) {
  plot <- patch_scale(
    plot, "x", ggplot2::scale_x_date, list(
    position = "bottom",
    limits = date_limits
  ), panel_name = "epicurve", call = rlang::caller_call())

  plot <- patch_scale(
    plot, "y", ggplot2::scale_y_continuous, list(
      # the documented default is 5% expansion, so replace now
      expand = ggplot2::expansion(0)
    ), panel_name = "epicurve", call = rlang::caller_call()
  )

  plot <- plot + ggplot2::theme(legend.position = "none")
  annotate_conditions_with_panel(plot, "epicurve")
}

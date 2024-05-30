#' Plot "phylepic" objects
#'
#' The `autoplot()` and `plot()` methods for "phylepic" objects assemble various
#' panels into the final plot. To facilitate customisations, the plots from
#' each panel can be overwritten. Some effort is made to ensure that the
#' specified plots will look reasonable when assembled.
#'
#' In general, if you wish to suppress a panel from the plot, set the
#' corresponding `plot.*` argument to `NULL`. To customise it, use the
#' corresponding `plot_*()` function, which returns a ggplot plot. You can then
#' add new layers or themes to that plot. See `vignette("phylepic")` for
#' examples.
#'
#' Legends from all panels are collected and de-duplicated. They are drawn on
#' the right edge of the overall plot.
#'
#' @param object,x Object of class "phylepic".
#' @param plot.tree ggplot for the tree panel (see [plot_tree]).
#' @param plot.bars ggplot for the metadata bars panel (see [plot_bars]).
#' @param plot.calendar ggplot for the calendar panel (see [plot_calendar]).
#' @param plot.epicurve ggplot for the epidemic curve panel (see [plot_epicurve]).
#' @param scale.date A date scale passed to both the calendar and epicurve panels
#'   (see [ggplot2::scale_x_date]).
#' @param scale.fill A fill scale passed to both the calendar and epicurve panels
#'   (see [ggplot2::scale_x_date]).
#' @param width.tree Relative width of the tree panel.
#' @param width.bars Relative width of the metadata bars panel.
#' @param width.date Relative width of the calendar panel.
#' @param width.legend Relative width of the legend, if present.
#' @param height.tree Relative height of the tree panel.
#' @param ... Ignored.
#'
#' @return `plot()` is usually called to display the plot, whereas `autoplot()`
#'   returns a "ggplot" object that can later be displayed with `print()`.
#' @family phylepic plots
#' @export
plot.phylepic <- function(
  x,
  ...,
  plot.tree = plot_tree(),
  plot.bars = plot_bars(),
  plot.calendar = plot_calendar(),
  plot.epicurve = plot_epicurve(),
  scale.date = NULL,
  scale.fill = NULL,
  width.tree = 10,
  width.bars = 1,
  width.date = 5,
  width.legend = 2,
  height.tree = 2
) {
  print(ggplot2::autoplot(
    object = x,
    ...,
    plot.tree = plot.tree,
    plot.bars = plot.bars,
    plot.calendar = plot.calendar,
    plot.epicurve = plot.epicurve,
    scale.date = scale.date,
    scale.fill = scale.fill,
    width.tree = width.tree,
    width.bars = width.bars,
    width.date = width.date,
    width.legend = width.legend,
    height.tree = height.tree
  ))
}

#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom rlang %||%
#' @rdname plot.phylepic
autoplot.phylepic <- function(
  object,
  ...,
  plot.tree = plot_tree(),
  plot.bars = plot_bars(),
  plot.calendar = plot_calendar(),
  plot.epicurve = plot_epicurve(),
  scale.date = NULL,
  scale.fill = NULL,
  width.tree = 10,
  width.bars = 1,
  width.date = 5,
  width.legend = 2,
  height.tree = 2
) {
  if (is.null(scale.date)) {
    scale.date <- ggplot2::scale_x_date(
      expand =  ggplot2::expansion(mult = c(0.1, 0.1)),
      oob = oob_infinite
    )
  }
  if (is.waive(scale.date$name)) {
    scale.date$name <- object$dates
  }

  guides <- list()

  plot.tree <- apply_plot(plot.tree, object)
  if (!is.null(plot.tree)) {
    plot.tree <- conform_plot_tree(plot.tree)
    guides <- c(guides, extract_guides(plot.tree))
  }

  relwidths <- c(width.tree)
  relheights <- c()
  hplotlist <- list(plot.tree)
  pos.calendar <- NULL

  plot.bars <- apply_plot(plot.bars, object)
  if (!is.null(plot.bars)) {
    plot.bars <- conform_plot_bars(plot.bars)
    guides <- c(guides, extract_guides(plot.bars))
    relwidths <- c(relwidths, width.bars)
    hplotlist[[length(hplotlist) + 1]] <- plot.bars
  }

  plot.calendar <- apply_plot(plot.calendar, object)
  if (!is.null(plot.calendar)) {
    plot.calendar <- conform_plot_calendar(
      plot.calendar,
      scale.date = scale.date,
      scale.fill = scale.fill,
      n = nrow(as.data.frame(object))
    )
    guides <- c(guides, extract_guides(plot.calendar))
    relwidths <- c(relwidths, width.date)
    pos.calendar <- length(hplotlist) + 1
    hplotlist[[pos.calendar]] <- plot.calendar
  }

  plot.epicurve <- apply_plot(plot.epicurve, object)
  if (!is.null(plot.epicurve)) {
    if (is.null(plot.calendar)) {
      cli::cli_abort("{.arg plot.calendar} cannot be {.code NULL} if {.arg plot.epicurve} is provided")
    }
    plot.epicurve <- conform_plot_epicurve(
      plot.epicurve,
      scale.date = scale.date,
      scale.fill = scale.fill
    )
    guides <- c(guides, extract_guides(plot.epicurve))
    relheights <- c(relheights, 1)
  }

  relheights <- c(relheights, height.tree)

  hplots <- cowplot::align_plots(plotlist = hplotlist, align = "h", axis = "tb")

  panel <- if (is.null(plot.epicurve)) {
    cowplot::plot_grid(plotlist = hplots, nrow = 1, rel_widths = relwidths)
  } else {
    vplots <- cowplot::align_plots(
      plot.epicurve,
      hplots[[pos.calendar]],
      align = "v",
      axis = "lr"
    )
    ncols <- length(hplots)
    plots <- vector("list", ncols * 2)
    plots[[ncols]] <- vplots[[1]]
    plots[(ncols + 1):(ncols * 2)] <- hplots
    plots[[ncols * 2]] <- vplots[[2]]

    cowplot::plot_grid(
      plotlist = plots,
      nrow = 2,
      rel_widths = relwidths,
      rel_heights = relheights
    )
  }

  if (!is.null(guides)) {
    # prepare the combined legends
    guides <- merge_guides(guides)
    theme <- (plot.bars %||% plot.epicurve %||% plot.tree %||% plot.calendar) |>
      extract_theme(complete = TRUE) +
      ggplot2::theme(legend.position = "right")

    # without the null device, draws an extra blank plot for some reason
    grDevices::pdf(file = nullfile())
    legends <- guides$assemble(theme)
    grDevices::dev.off()

    panel <- cowplot::plot_grid(
      panel,
      legends$right,
      rel_widths = c(width.tree, width.legend)
    )
  }

  panel
}

apply_plot <- function(plot_cfg, data) {
  if (rlang::is_callable(plot_cfg)) plot_cfg(data) else plot_cfg
}

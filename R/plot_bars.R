#' Plot metadata bars panel
#'
#' This uses [ggplot2::geom_tile()] to produce a grid with a row aligned with
#' each tip on the tree, and a column for each type of data specified. If no
#' scales are specified, one is created for each factor column in the metadata
#' table.
#'
#' @param phylepic object of class "phylepic".
#' @param ... scale specifications.
#'
#' @inherit plot_tree return
#' @family phylepic plots
#' @export
plot_bars <- function(phylepic, ...) {
  wrapper <- function(x) {
    x <- as.data.frame(x)
    stopifnot(is.tip_data(x))

    scales <- rlang::list2(...)
    if (length(scales) == 0) scales <- guess_scales(x)

    p <-
      ggplot2::ggplot(x) +
      ggplot2::scale_x_discrete() +
      ggplot2::scale_y_continuous(
        breaks = NULL,
        expand = ggplot2::expansion(add = 0)
      )

    for (i in seq_len(length(scales))) {
      name <- names(scales)[[i]]
      col <- x[[name]]
      if (! is.factor(col)) {
        cli::cli_abort(c(
          "!" = "{.fn plot_bars} requires factor columns",
          "x" = "Column {.field {name}} is {.obj_type_friendly {col}}, not a factor"
        ))
      }

      if (!any(ggplot2::standardise_aes_names(scales[[i]]$aesthetics) %in% c("fill", "colour"))) {
        cli::cli_warn(
          "Scale provided for bar {name} is not a colour scale, but is defined for {scales[[i]]$aesthetics}"
        )
      }
      scales[[i]]$aesthetics <- "fill"

      if (i > 1) p <- p + ggnewscale::new_scale_fill()
      p <- p + scales[[i]]

      m <- aes(y = .data$.phylepic.index)
      m$x <- if (is.waive(scales[[i]]$name)) name else scales[[i]]$name
      m$fill <- rlang::sym(name)
      m$colour <- rlang::quo(dplyr::if_else(
        is.na(.data[[name]]),
        I(NA_character_),
        I("grey30")
      ))
      p <- p + ggplot2::geom_tile(
        mapping = m, width = 1, height = 1, linewidth = 0.3
      )
    }

    p +
      theme_plot_bars() +
      coord_tree()
  }

  plot <- if (missing(phylepic)) wrapper else wrapper(phylepic)
  annotate_conditions_with_panel(plot, "bars")
}

guess_scales <- function(data) {
  data <- as.data.frame(data)

  factors <- dplyr::select(data, dplyr::where(is.factor))
  if (length(factors) == 0) {
    cli::cli_abort(c(
      "!" = "Unable to guess scales for {.fn plot_bars}",
      "x" = "None of the columns in the metadata data frame are factors",
      "i" = "Use {.code plot.bars = plot_bars(...)} to explicitly provide scales",
      "i" = "Alternatively use {.code plot.bars = NULL} to remove the bar plot"
    ))
  }

  guess <- list()
  for (i in seq_len(ncol(factors))) {
    scale_name <- colnames(factors)[i]
    guess[[scale_name]] <- ggplot2::scale_fill_brewer(
      type = "qual",
      palette = (i - 1L) %% 8L + 1L,
      drop = FALSE,
      na.translate = FALSE,
    )
  }

  guess
}

theme_plot_bars <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(l = unit(2, "pt"), r = 0),
  )
}

conform_plot_bars <- function(plot) {
  # The y scale expansion must match or else the plots won't line up.
  plot <- mutate_scale(plot, "y", ggplot2::scale_y_continuous(), f = function(scale) {
    scale$expand <- ggplot2::expansion(add = 0)
    scale
  })

  plot <- plot + ggplot2::theme(legend.position = "none")
  annotate_conditions_with_panel(plot, "bars")
}

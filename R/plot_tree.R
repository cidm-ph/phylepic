#' Plot phylogenetic tree panel
#'
#' The tree is drawn using `ggraph` with its dendrogram layout. When
#' customising it, you may wish to add layers such as
#' [ggraph::geom_node_point()].
#' The metadata table is joined onto the tree, so all its column names are
#' available for use in the various `ggraph` geoms.
#'
#' @param phylepic object of class "phylepic".
#' @param label variable in metadata table corresponding to the tip labels (tidy-eval).
#' @param bootstrap when `TRUE`, draw bootstrap vaues on the tree. These are
#'   only drawn if they are detected from the node labels having the form "a/b"
#'   where both "a" and "b" are numbers. Currently, the bootstrap values are
#'   displayed as a percentage, suppressing zero values and values for very
#'   short branches. To customise the appearance or details instead use
#'   `bootstrap = FALSE` and add your own layer with [ggraph::geom_edge_elbow].
#'
#' @return If `phylepic` is specified returns a ggplot, otherwise a function
#'  that when passed a "phylepic" object produces a ggplot for use with
#'  [plot.phylepic()].
#' @family phylepic plots
#' @export
plot_tree <- function(phylepic, label = .data$name, bootstrap = TRUE) {
  wrapper <- function(x) {
    tip_data <- NULL
    if (is.phylepic(x)) tip_data <- as.data.frame(x)

    tr_layout <- create_tree_layout(x, tip_data)
    x_width <- max(tr_layout$x)

    p <- ggplot2::ggplot(tr_layout) +
      geom_node_text_filled(
        aes(label = {{label}}, filter = .data$leaf),
        size = 3,
        hjust = 0,
        position = ggplot2::position_nudge(x = 0.01 * x_width),
        colour = "#666666"
      ) +
      ggraph::geom_edge_elbow(flipped = TRUE)

    if (bootstrap && (".phylepic.bootstrap_numeric" %in% colnames(tr_layout))) {
      p <- p + ggraph::geom_edge_elbow(
        aes(label = round(.data$node2..phylepic.bootstrap_numeric, digits = 0)),
        data = function(layout) {
          ggraph::get_edges()(layout) |>
            dplyr::filter(
              (!.data$node2.leaf),
              (length >= 0.05 * max(length)),
              round(.data$node2..phylepic.bootstrap_numeric, digits = 0) > 0
            )
        },
        flipped = TRUE,
        label_pos = 0.75,
        label_size = 3,
        angle_calc = "along",
        vjust = -0.2,
        colour = NA
      )
    }

    p +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.2)),
        transform = "reverse"
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.5)) +
      theme_plot_tree() +
      coord_tree()
  }

  plot <- if (missing(phylepic)) wrapper else wrapper(phylepic)
  annotate_conditions_with_panel(plot, "tree")
}

theme_plot_tree <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(l = 0, r = 0),
  )
}

conform_plot_tree <- function(plot) {
  # The y scale expansion must match or else the plots won't line up.
  plot <- mutate_scale(plot, "y", ggplot2::scale_y_continuous(), f = function(scale) {
    scale$expand <- ggplot2::expansion(add = 0.5)
    scale
  })

  if (!inherits(plot$coordinate, "CoordTree")) {
    cli::cli_inform("adding {.fn coord_tree} to {.arg plot.tree}")
    plot <- plot + coord_tree()
  }

  scale.x <- plot$scales$get_scales("x")
  if (scale.x$trans$name != "reverse") {
    cli::cli_inform("adding {.fn scales::reverse_trans} to {.arg plot.tree}")
    plot <- mutate_scale(plot, "x", ggplot2::scale_x_continuous(), f = function(scale) {
      scale$trans <- scales::reverse_trans()
      scale
    })
  }

  lo_errors <- c()
  if (! inherits(plot$data, "layout_tbl_graph")) {
    lo_errors <- c(lo_errors, "x" = "{.arg plot.tree} must be a graph layout prepared by {.pkg ggraph}")
  }
  graph <- attr(plot$data, "graph")
  if (!tidygraph::with_graph(graph, tidygraph::graph_is_tree())) {
    lo_errors <- c(lo_errors, "x" = "{.arg plot.tree} uses a graph that is not a tree")
  }
  if (layout_is_circular(plot$data)) {
    lo_errors <- c(lo_errors, "x" = "{.arg plot.tree} is circular")
  }
  if (!layout_is_horizontal(plot$data)) {
    lo_errors <- c(lo_errors, "x" = "{.arg plot.tree} does not look like a horizontal tree")
  }
  if (length(lo_errors) > 0) {
    cli::cli_abort(c(
      "!" = "The tree layout does not match what {.fn phylepic} expects to find",
      lo_errors,
      "i" = "Use {.fn create_tree_layout} to create a suitable layout for {.fn ggraph::ggraph}"
    ))
  }

  plot <-
    plot +
    warn_theme(
      ggplot2::theme_get() + plot$theme,
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      .name = "plot.tree"
    ) +
    ggplot2::theme(legend.position = "none")

  annotate_conditions_with_panel(plot, "tree")
}

layout_is_circular <- function(layout) {
  attr(layout, "circular")
}

layout_is_horizontal <- function(layout) {
  leaves <- layout$y[layout$leaf]
  setequal(leaves, seq_len(length(leaves)) - 1)
}

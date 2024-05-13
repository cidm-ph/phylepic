#' Cartesian coordinates with specialised grid for trees
#'
#' This coord is based on the default Cartesian coordinates, but draws the a
#' filled background in addition to the normal grid lines. The grid is forced
#' to appear on every integer value within the scale's range.
#'
#' The appearance of the grid can be controlled with theme elements:
#' \describe{
#'   \item{`phylepic.grid.bar`}{filled grid (`element_rect()`).}
#'   \item{`phylepic.grid.line`}{grid line (`element_line()`).}
#'   \item{`phylepic.grid.every`}{grid frequency (`integer`). Default for both
#'         `phylepic.grid.every.bar` and `phylepic.grid.every.stripe`}
#'   \item{`phylepic.grid.every.bar`}{grid bar frequency (`integer`).
#'         Defaults to 2 to give an alternative striped background}
#'   \item{`phylepic.grid.every.stripe`}{grid bar frequency (`integer`).
#'         Defaults to 1 so that every tip on a tree has its own line}
#' }
#'
#' @param xlim,ylim,expand,default,clip See `ggplot2::coord_cartesian()`
#'
#' @return coord suitable for adding to a plot
#' @export
coord_tree <- function(
  xlim = NULL, ylim = NULL, expand = TRUE, default = FALSE, clip = "on"
) {
  ggplot2::ggproto(NULL, CoordTree,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname coord_tree
CoordTree <- ggplot2::ggproto("CoordTree", ggplot2::CoordCartesian,
  render_bg = function(panel_params, theme) {
    stripe_freq <- ggplot2::calc_element("phylepic.grid.every.bar", theme)
    stripes_y <- seq(
      panel_params$y.range[[1]],
      panel_params$y.range[[2]] - 1,
      by = stripe_freq
    )
    stripes_y <- panel_params$y$rescale(ceiling(stripes_y))

    line_freq <- ggplot2::calc_element("phylepic.grid.every.line", theme)
    lines_y <- seq(
      panel_params$y.range[[1]],
      panel_params$y.range[[2]] - 1,
      by = line_freq
    )
    lines_y <- panel_params$y$rescale(ceiling(lines_y))

    grid::grobTree(
      ggplot2::element_render(theme, "phylepic.grid.bar",
        x = 0.5,
        width = 1,
        y = stripes_y,
        height = panel_params$y$rescale(0.5),
        linetype = 0,
        colour = NA
      ),
      ggplot2::element_render(theme, "phylepic.grid.line",
        x = rep(1:0, length(lines_y)),
        y = rep(lines_y, each = 2),
        id.lengths = rep(2, length(lines_y))
      ),
      ggplot2::CoordCartesian$render_bg(panel_params, theme)
    )
  },
)

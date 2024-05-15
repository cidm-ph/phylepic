#' Annotate nodes with text and a background
#'
#' This geom behaves like `ggraph::geom_node_text()` except that it also inserts
#' a white background behind the text extending to the left margin. This will
#' only make sense for a horizontal dendrogram graph layout with the root node
#' on the left.
#'
#' This background covers up part of the grid rendered by the coord layer.
#' The reason that this is done as part of the text instead of as a separate
#' layer is so that we have access to the rendered dimensions of the text grobs.
#'
#' @param mapping,data,position,parse,check_overlap,show.legend,...
#'   Arguments passed to the geom that powers `ggraph::geom_node_text()`.
#'   Note that the additional arguments of that function such as `repel` are
#'   not supported here.
#'
#' @return Layer that draws text and background grobs
#' @export
geom_node_text_filled <- function(
  mapping = NULL, data = NULL, position = "identity", parse = FALSE,
  check_overlap = FALSE, show.legend = NA, ...
) {
  params <- rlang::list2(parse = parse, ...)
  stat <- ggraph::StatFilter
  geom <- GeomTextFilled
  params$check_overlap <- check_overlap
  if (check_overlap && is.null(data)) {
    stat <- ggraph::StatReverse
  }

  mapping <- aes_intersect(mapping, aes(x = .data$x, y = .data$y))
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = params
  )
}

#' @export
#' @usage NULL
#' @format NULL
#' @rdname geom_node_text_filled
GeomTextFilled <- ggplot2::ggproto("GeomTextFilled", ggplot2::GeomText,
  required_aes = c("x", "y"),
  optional_aes = c("label"),

  draw_panel = function(data, panel_params, coord, ...) {
    data$label[is.na(data$label)] <- ""
    grob <- ggplot2::GeomText$draw_panel(data, panel_params, coord, ...)

    if (length(grob$y) < 2) {
      stop("Need at least 2 rows in text dataset for GeomTextFilled")
    }

    widths <- map_unit(grob$label, ~ unit(1, "strwidth", data = .x))
    right <- map2_unit(grob$x, widths * 0.75, `+`)
    order <- order(grid::convertY(grob$y, "native", valueOnly = TRUE))

    height_full <- grid::convertHeight(grob$y[order][[2]] - grob$y[order][[1]], "native")

    bg_grob <- grid::polygonGrob(
      x = grid::unit.c(
        unit(0, "native"),
        rep(right[order], each = 2),
        unit(0, "native")
      ),
      y = grid::unit.c(
        rep(grob$y[order] - height_full / 2, each = 2),
        unit(c(1, 1), "native")
      ),
      gp = grid::gpar(fill = "white", col = NA, linetype = 0)
    )

    ggname("geom_node_text_filled", grid::grobTree(bg_grob, grob))
  },

  parameters = function(self, extra = FALSE) {
    # report that we support the same arguments that GeomText does
    args <- names(formals(environment(ggplot2::GeomText$draw_panel)$f))
    if (extra) {
      args <- union(args, self$extra_params)
    }
    args
  }
)

map_unit <- function(.x, .f) {
  .f <- rlang::as_function(.f)
  Reduce(grid::unit.c, mapply(.f, .x, SIMPLIFY = FALSE))
}

map2_unit <- function(.x, .y, .f) {
  .f <- rlang::as_function(.f)
  Reduce(grid::unit.c, mapply(.f, .x, .y, SIMPLIFY = FALSE))
}

interleave_unit <- function(.x, .y) {
  n <- length(.x)
  stopifnot(length(.y) == n)
  Reduce(
    grid::unit.c,
    Map(function(i) grid::unit.c(.x[[i]], .y[[i]]), seq_len(n))
  )
}

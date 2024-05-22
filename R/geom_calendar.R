#' @rdname geom_calendar
#' @format NULL
#' @usage NULL
#' @importFrom rlang %||%
#' @export
GeomCalendar <- ggplot2::ggproto("GeomCalendar", ggplot2::GeomTile,
  default_aes = aes(fill = "grey20", colour = NA, linewidth = 0.1, linetype = 1,
                    alpha = NA, width = NA, height = NA),
  required_aes = c("x", "y"),
  optional_aes = c("label"),
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  extra_params = c("na.rm"),
  rename_size = TRUE,
  draw_key = ggplot2::draw_key_polygon,

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||%
      ggplot2::resolution(data$x, FALSE)
    data$height <- data$height %||%
      params$height %||%
      ggplot2::resolution(data$y, FALSE)

    transform(
      data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  draw_panel = function(
    self, data, panel_params, coord, lineend = "butt", linejoin = "mitre",
    label_params = list(colour = "grey30")
  ) {
    inf <- data[is.infinite(data$x), ]
    data <- data[is.finite(data$x), ]
    coords <- coord$transform(data, panel_params)

    tiles <- grid::rectGrob(
      coords$xmin,
      coords$ymax,
      width = coords$xmax - coords$xmin,
      height = coords$ymax - coords$ymin,
      default.units = "native",
      just = c("left", "top"),
      gp = grid::gpar(
        col = coords$colour,
        fill = ggplot2::fill_alpha(coords$fill, coords$alpha),
        lwd = coords$linewidth * ggplot2::.pt,
        lty = coords$linetype,
        linejoin = linejoin,
        lineend = lineend
      )
    )

    names(label_params) <- ggplot2::standardise_aes_names(names(label_params))
    if ("colour" %in% names(label_params) && is.na(label_params$colour)) {
      cli::cli_warn(c(
        "Trying to draw labels with missing colour ({.fn geom_calendar})",
        "i" = 'Set label colour with {.code label_params = list(colour = "grey20", ...)}'
      ))
    }

    grobs <- if ("label" %in% colnames(data)) {
      text_data <- ggplot2::GeomText$use_defaults(data, label_params)
      labels <- ggplot2::GeomText$draw_panel(text_data, panel_params, coord)
      grid::gList(tiles, labels)
    } else {
      tiles
    }

    if (nrow(inf) > 0) {
      width <- median(data$xmax - data$xmin, na.rm = TRUE) / 2
      x_range <- coord$limits$x %||% panel_params$x$scale$limits

      inf$xmin <- ifelse(inf$x < 0, x_range[[1]] - width, x_range[[2]])
      inf$xmax <- ifelse(inf$x < 0, x_range[[1]], x_range[[2]] + width)
      inf$x <- ifelse(inf$x < 0, inf$xmax, inf$xmin)
      inf <- coord$transform(inf, panel_params)

      x_tip <- ifelse(inf$x == inf$xmin, inf$xmax, inf$xmin)
      x_base <- ifelse(inf$x == inf$xmin, inf$xmin, inf$xmax)
      arr_fill <- ggplot2::fill_alpha(inf$fill, inf$alpha)
      arr_colour <- inf$colour
      arr_blank <- is.na(inf$colour) & is.na(arr_fill)
      if (any(arr_blank)) {
        cli::cli_warn(
          "Dropped {sum(arr_blank)} calendar arrows with no fill or colour ({.fn geom_calendar})",
        )
      }
      arrows <- grid::polygonGrob(
        x = c(rbind(x_tip, x_base, x_base)),
        y = c(rbind(inf$y, inf$ymax, inf$ymin)),
        id = rep(seq_len(nrow(inf)), each = 3),
        default.units = "native",
        gp = grid::gpar(
          col = arr_colour,
          fill = arr_fill,
          lwd = inf$linewidth * ggplot2::.pt,
          lty = inf$linetype,
          linejoin = linejoin,
          lineend = lineend
        )
      )

      grobs <- grid::gList(grobs, arrows)
    }

    ggname("geom_calendar", grid::grobTree(grobs))
  }

)

#' Specialised tile geometry for calendar plots
#'
#' This geom behaves mostly the same as [ggplot2::geom_tile()] with a few
#' additions. Firstly, the `label` aesthetic is supported to draw text on top of
#' the tiles. Secondly, out of bounds values can be drawn as arrows at the edge
#' of the scale (see details below).
#'
#' Any `x` values that are infinite (i.e. `-Inf` or `Inf`) would normally be
#' dropped by ggplot's layers. If any such values survive the stat processing,
#' they will be drawn by `geom_calendar()` as triangles at the respective edges
#' of the scale. This is intended to work with a scale configured to use
#' [oob_infinite()] for out of bounds handling.
#' The triangles are drawn with their base (vertical edge) sitting on the scale
#' limit, and their width equal to half of the median bin width.
#'
#' Note that the `label` aesthetic will be dropped if the data are not grouped
#' in the expected way. In general this means that all rows contributing to a
#' given bin must have the same value for the `label` aesthetic.
#'
#' @param label_params additional parameters for text labels if present
#'   (see [ggplot2::geom_text()]).
#' @param mapping,data,stat,position,linejoin,na.rm,show.legend,inherit.aes,...
#'   see [ggplot2::geom_tile()].
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' set.seed(1)
#' events <- rep(as.Date("2024-01-31") - 0:30, rpois(31, 6))
#' values <- round(rgamma(length(events), 1, 0.01))
#' df <- data.frame(date = events, value = values)
#'
#' ggplot(df) +
#'     geom_calendar(
#'         aes(date, value, label = after_stat(count)),
#'         colour = "white",
#'         stat = "week_2d",
#'         week_start = "Monday",
#'         bins.y = 10
#'     ) +
#'     scale_x_week(
#'         limits = as.Date(c("2024-01-08", NA)),
#'         expand = expansion(add = 3.5)
#'     )
geom_calendar <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  linejoin = "mitre",
  label_params = list(colour = "grey30"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCalendar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      linejoin = linejoin,
      label_params = label_params,
      na.rm = na.rm,
      ...
    )
  )
}

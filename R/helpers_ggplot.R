#' Out of bounds handling
#'
#' This helper works the same way as [scales::oob_censor()] and similar. Out of
#' bounds values are pushed to positive or negative infinity. This is not useful
#' for builtin ggplot layers which will display a warning and drop rows with
#' infinite values in required aesthetics. [geom_calendar()] however uses the
#' infinite values to indicate out of bounds values explicitly on the plot.
#'
#' @param x A numeric vector of values to modify.
#' @param range A numeric vector of length two giving the minimum and maximum
#'   limit of the desired output range respectively.
#'
#' @rdname oob
#' @export
oob_infinite <- function(x, range = c(0, 1)) {
  force(range)
  x[x < range[1]] <- -Inf
  x[x > range[2]] <- +Inf
  x
}

replace_scale <- function(plot, scale) {
  old.scale <- plot$scales$get_scales(scale$aesthetics)
  if (!is.null(old.scale)) {
    prev_aes <- plot$scales$find(scale$aesthetics)
    plot$scales$scales <- plot$scales$scales[!prev_aes]
  }
  plot + scale
}

mutate_scale <- function(plot, aesthetic, default_scale = NULL, f) {
  f <- rlang::as_function(f)

  old.scale <- plot$scales$get_scales(aesthetic)
  if (is.null(old.scale)) {
    if (is.null(default_scale)) return(plot)
    old.scale <- default_scale
  } else {
    prev_aes <- plot$scales$find(old.scale$aesthetics)
    plot$scales$scales <- plot$scales$scales[!prev_aes]
  }
  plot + f(old.scale)
}

warn_theme <- function(plot_theme, ..., .name) {
  settings <- rlang::list2(...)

  for (i in seq_along(settings)) {
    key <- names(settings)[[i]]
    val <- unname(settings[[i]])
    elem <- ggplot2::calc_element(key, plot_theme)
    if (!inherits(elem, class(val)[[1]])) {
      cli::cli_inform(
        "{.arg {name}} theme: overwriting {.arg {key}} to {.obj_type_friendly {val}}"
      )
    }
  }

  ggplot2::theme(!!!settings)
}

extract_theme <- function(plot, complete = FALSE) {
  if (!inherits(plot, "ggplot_built")) plot <- ggplot2::ggplot_build(plot)
  theme <- plot$plot$theme

  # ggplot2 does more than this internally to complete themes...
  if (complete) theme <- ggplot2::theme_get() + theme

  theme
}

extract_guides <- function(plot) {
  if (!inherits(plot, "ggplot_built")) plot <- ggplot2::ggplot_build(plot)
  plot$plot$guides
}

merge_guides <- function(guides) {
  if (length(guides) < 1L) {
    NULL
  } else if (length(guides) == 1L) {
    guides[[1]]
  } else {
    guides <- Reduce(
      concat_guides,
      x = guides[2:length(guides)],
      init = guides[[1]]
    )
    guides$merge()
    guides
  }
}

concat_guides <- function(acc, nxt) {
  acc$guides <- c(acc$guides, nxt$guides)
  acc$aesthetics <- c(acc$aesthetics, nxt$aesthetics)
  acc$params <- c(acc$params, nxt$params)
  acc
}

# The below are inlined from ggplot2 3.5.0

is.waive <- function(x) {
  inherits(x, "waiver")
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

aes_intersect <- function(aes1, aes2) {
  aes <- c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)])
  class(aes) <- "uneval"
  aes
}

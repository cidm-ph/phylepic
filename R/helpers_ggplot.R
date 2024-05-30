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
#' @return A numerical vector of the same length as `x` where out of bound
#'   values have been replaced by `Inf` or `-Inf` accordingly.
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
  withCallingHandlers(
    plot <- ggplot2::ggplot_build(plot),
    warning = function(cnd) {
      # any warnings will be displayed in the final build, no need to duplicate
      invokeRestart("muffleWarning")
    }
  )
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
    withCallingHandlers(
      guides$merge(),
      message = function(cnd) {
        if (inherits(cnd, "rlib_message_name_repair")) {
          # these are un-actionable messages due to scale names clashing
          invokeRestart("muffleMessage")
        }
      },
      warning = function(cnd) {
        if (inherits(cnd, "rlang_warning") && grepl("Duplicated `override.aes`", cnd$message)) {
          # these are raised already when constructing the individual panels
          invokeRestart("muffleWarning")
        }
      }
    )
    guides
  }
}

concat_guides <- function(acc, nxt) {
  acc$guides <- c(acc$guides, nxt$guides)
  acc$aesthetics <- c(acc$aesthetics, nxt$aesthetics)
  acc$params <- c(acc$params, nxt$params)
  acc
}

annotate_conditions_with_panel <- function(plot, panel_name) {
  attr(plot, "phylepic.panel") <- panel_name
  if (!inherits(plot, "phylepic_ggplot"))
    class(plot) <- c("phylepic_ggplot", class(plot))
  plot
}

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.phylepic_ggplot <- function(plot) {
  panel_name <- attr(plot, "phylepic.panel")
  build <- withCallingHandlers(
    NextMethod(generic = "ggplot_build", object = plot),
    message = annotate_and_reraise(panel_name, "muffleMessage"),
    warning = annotate_and_reraise(panel_name, "muffleWarning"),
    error = annotate_and_reraise(panel_name)
  )
  class(build) <- c("phylepic_ggplot_build", class(build))
  attr(build, "phylepic.panel") <- panel_name
  build
}

#' @importFrom ggplot2 ggplot_gtable
#' @export
ggplot_gtable.phylepic_ggplot_build <- function(data) {
  panel_name <- attr(data, "phylepic.panel")
  withCallingHandlers(
    NextMethod(generic = "ggplot_gtable", object = data),
    message = annotate_and_reraise(panel_name, "muffleMessage"),
    warning = annotate_and_reraise(panel_name, "muffleWarning"),
    error = annotate_and_reraise(panel_name)
  )
}

annotate_and_reraise <- function(panel_name, restart = NULL) {
  function(cnd) {
    if (inherits(cnd, c("rlang_error", "rlang_warning", "rlang_message"))) {
      cnd$message <- paste0(cnd$message, " [phylepic: plot.", panel_name, "]")
      rlang::cnd_signal(cnd)
      if (!is.null(restart)) invokeRestart(restart)
    }
  }
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

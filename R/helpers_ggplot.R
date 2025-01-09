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
  if (is.null(scale)) return(plot)
  old.scale <- plot$scales$get_scales(scale$aesthetics)
  if (!is.null(old.scale)) {
    prev_aes <- plot$scales$find(scale$aesthetics)
    plot$scales$scales <- plot$scales$scales[!prev_aes]
  }
  plot + scale
}

#' @noRd
#' @param plot a ggplot plot
#' @param aesthetic string naming the aes, e.g. `"x"`
#' @param constructor function that creates the default scale, e.g. `scale_x_continuous`
#' @param params list of values to set on the scale
#' @param call override the call when calling this from an internal helper function
patch_scale <- function(
  plot, aesthetic, constructor, params = list(),
  call = rlang::caller_call(), panel_name = "<unknown>"
) {
  old.scale <- plot$scales$get_scales(aesthetic)
  if (is.null(old.scale)) {
    old.scale <- constructor()
  } else {
    # remove the existing scale to prevent scale replacement warning
    prev_aes <- plot$scales$find(old.scale$aesthetics)
    plot$scales$scales <- plot$scales$scales[!prev_aes]

    new.class <- class(constructor())
    if (!inherits(old.scale, new.class[[1]])) {
      cli::cli_warn(paste0(
        "Scale is of the wrong type, {.cls {class(old.scale)[[1]]}}; ",
        "replacing it with new {.cls {new.class[[1]]}}"
      ), call = call)
      old.scale <- constructor()
    }
  }

  for (k in names(params)) {
    old.call <- substitute(old.scale)
    new.call <- substitute(params)
    new.value <- params[[k]]
    if (!identical(old.scale[[k]] , new.value, ignore.environment = TRUE)) {
      cli::cli_warn(paste0(
        "Replaced {.val {aesthetic}} scale {.field {k}}: ",
        "{cli::code_highlight(deparse1(old.call[[k]]))} -> ",
        "{cli::code_highlight(deparse1(new.call[[k]]))} ",
        "[phylepic: plot.{panel_name}]"
      ), call = call)
      old.scale[[k]] <- new.value
    }
  }

  plot + old.scale
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

breaks_from_scale <- function(breaks, scale) {
  major_breaks <- scale$get_breaks()
  minor_breaks <- scale$get_breaks_minor()

  # clamp to limits
  # FIXME this might be too aggressive for bins at the limits
  limits <- scale$get_limits()
  major_breaks <- major_breaks[(major_breaks >=  limits[[1]]) & (major_breaks <= limits[[2]])]
  minor_breaks <- minor_breaks[(minor_breaks >=  limits[[1]]) & (minor_breaks <= limits[[2]])]

  breaks <- switch(breaks,
                   minor = minor_breaks,
                   major = major_breaks,
                   all = unique(sort(c(minor_breaks, major_breaks))))
  scale$get_transformation()$inverse(breaks)
}

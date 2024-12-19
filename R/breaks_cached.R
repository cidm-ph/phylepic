#' Cached scale break function
#'
#' This helper caches the output of a breaks function (such as
#' `scales::breaks_width()`). This means that the first time the breaks are
#' computed with the helper, the resulting breaks vector will be stored.
#' All subsequent invocations of the helper will return the same stored breaks,
#' regardless of the limits provided.
#'
#' In general this is not what you want, since the breaks should change when the
#' limits change.
#'
#' @param breaks A function that takes the limits as input and returns breaks
#'   as output. See `ggplot2::continuous_scale` for details.
#'
#' @return A wrapped breaks function suitable for use with ggplot scales.
#' @export
breaks_cached <- function(breaks) {
  if (! rlang::is_function(breaks)) {
    cli::cli_abort("{.arg breaks} must be a function")
  }
  cache <- NULL
  cached <- function(...) {
    if (is.null(cache)) {
      cache <<- breaks(...)
    }
    cache
  }

  class(cached) <- c("phylepic_cached_breaks", class(cached))
  cached
}

#' @export
format.phylepic_cached_breaks <- function(x, ...) {
  bc <- environment(x)

  paste0(
    "<cached breaks function>\n",
    ifelse(
      is.null(bc$cache),
      paste0("  ", format(bc$breaks), collapse = "\n"),
      paste0("  [", class(bc$cache), "] ", paste0(format(bc$cache), collapse = " "))
    )
  )
}

#' @export
print.phylepic_cached_breaks <- function(x, ...) {
  cat(format(x), sep = "")
}

# This file was extracted from ggplot2 version 4.5.0. It is unchanged from the
# original except for commenting out print.ggplot2_bins and some internal
# ggplot2 numeric checks or namespace adjustments (all changes marked with #@).
#
# Copyright (c) 2020 ggplot2 authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


bins <- function(breaks, closed = "right",
                 fuzz = 1e-08 * stats::median(diff(breaks))) {
  #@ check_numeric(breaks)
  #@ closed <- arg_match0(closed, c("right", "left"))
  closed <- rlang::arg_match0(closed, c("right", "left"))#@

  breaks <- sort(breaks)
  # Adapted base::hist - this protects from floating point rounding errors
  if (closed == "right") {
    fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1))
  } else {
    fuzzes <- c(rep.int(-fuzz, length(breaks) - 1), fuzz)
  }

  structure(
    list(
      breaks = breaks,
      fuzzy = breaks + fuzzes,
      right_closed = closed == "right"
    ),
    class = "ggplot2_bins"
  )
}

# Compute parameters -----------------------------------------------------------

bin_breaks <- function(breaks, closed = c("right", "left")) {
  bins(breaks, closed)
}

bin_breaks_width <- function(x_range, width = NULL, center = NULL,
                             boundary = NULL, closed = c("right", "left")) {
  if (length(x_range) != 2) {
    cli::cli_abort("{.arg x_range} must have two elements.")
  }

  # binwidth seems to be the argument name supplied to width. (stat-bin and stat-bindot)
  #@ check_number_decimal(width, min = 0, allow_infinite = FALSE, arg = "binwidth")

  if (!is.null(boundary) && !is.null(center)) {
    cli::cli_abort("Only one of {.arg boundary} and {.arg center} may be specified.")
  } else if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2

    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }

  # Find the left side of left-most bin: inputs could be Dates or POSIXct, so
  # coerce to numeric first.
  x_range <- as.numeric(x_range)
  width <- as.numeric(width)
  boundary <- as.numeric(boundary)
  shift <- floor((x_range[1] - boundary) / width)
  origin <- boundary + shift * width

  # Small correction factor so that we don't get an extra bin when, for
  # example, origin = 0, max(x) = 20, width = 10.
  max_x <- x_range[2] + (1 - 1e-08) * width

  if (isTRUE((max_x - origin) / width > 1e6)) {
    cli::cli_abort(c(
      "The number of histogram bins must be less than 1,000,000.",
      "i" = "Did you make {.arg binwidth} too small?"
    ))
  }
  breaks <- seq(origin, max_x, width)

  if (length(breaks) == 1) {
    # In exceptionally rare cases, the above can fail and produce only a
    # single break (see issue #3606). We fix this by adding a second break.
    breaks <- c(breaks, breaks + width)
  }

  bin_breaks(breaks, closed = closed)
}

bin_breaks_bins <- function(x_range, bins = 30, center = NULL,
                            boundary = NULL, closed = c("right", "left")) {
  if (length(x_range) != 2) {
    cli::cli_abort("{.arg x_range} must have two elements.")
  }

  #@ check_number_whole(bins, min = 1)
  #@ if (zero_range(x_range)) {
  if (scales::zero_range(x_range)) {#@
    # 0.1 is the same width as the expansion `default_expansion()` gives for 0-width data
    width <- 0.1
  } else if (bins == 1) {
    width <- diff(x_range)
    boundary <- x_range[1]
  } else {
    width <- (x_range[2] - x_range[1]) / (bins - 1)
  }

  bin_breaks_width(
    x_range, width, boundary = boundary, center = center, closed = closed
  )
}

# ------------------------------------------------------------------------
# The remainder of the file is reproduced from the unreleased ggplot2 code
# available at https://github.com/tidyverse/ggplot2/pull/6212/

compute_bins <- function(x, scale = NULL, breaks = NULL, binwidth = NULL, bins = NULL,
                         center = NULL, boundary = NULL,
                         closed = c("right", "left")) {

  range <- if (is.scale(scale)) scale$dimension() else range(x)
  #@ check_length(range, 2L)

  if (!is.null(breaks)) {
    breaks <- allow_lambda(breaks)
    if (is.function(breaks)) {
      breaks <- breaks(x)
    }
    if (is.scale(scale) && !scale$is_discrete()) {
      breaks <- scale$transform(breaks)
    }
    #@ check_numeric(breaks)
    bins <- bin_breaks(breaks, closed)
    return(bins)
  }

  #@ check_number_decimal(boundary, allow_infinite = FALSE, allow_null = TRUE)
  #@ check_number_decimal(center, allow_infinite = FALSE, allow_null = TRUE)
  if (!is.null(boundary) && !is.null(center)) {
    cli::cli_abort("Only one of {.arg boundary} and {.arg center} may be specified.")
  }

  if (!is.null(binwidth)) {
    binwidth <- allow_lambda(binwidth)
    if (is.function(binwidth)) {
      binwidth <- binwidth(x)
    }
    #@ check_number_decimal(binwidth, min = 0, allow_infinite = FALSE)
    bins <- bin_breaks_width(
      range, binwidth,
      center = center, boundary = boundary, closed = closed
    )
    return(bins)
  }

  bins <- allow_lambda(bins)
  if (is.function(bins)) {
    bins <- bins(x)
  }
  #@ check_number_whole(bins, min = 1, allow_infinite = FALSE)
  bin_breaks_bins(
    range, bins,
    center = center, boundary = boundary, closed = closed
  )
}

bin_cut <- function(x, bins) {
  cut(x, bins$fuzzy, right = bins$right_closed, include.lowest = TRUE)
}

bin_loc <- function(x, id) {
  left <- x[-length(x)]
  right <- x[-1]

  list(
    left = left[id],
    right = right[id],
    mid = ((left + right) / 2)[id],
    length = diff(x)[id]
  )
}

# This functions below were taken unaltered from ggplot2 version 4.5.0.
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

dual_param <- function(x, default = list(x = NULL, y = NULL)) {
  if (is.null(x)) {
    default
  } else if (length(x) == 2) {
    if (is.list(x) && !is.null(names(x))) {
      x
    } else {
      list(x = x[[1]], y = x[[2]])
    }
  } else {
    list(x = x, y = x)
  }
}

allow_lambda <- function(x) {
 if (rlang::is_formula(x)) rlang::as_function(x) else x
}

is.scale <- function(x) inherits(x, "Scale")

#' Combine metadata (a line list) with a phylogenetic tree
#'
#' Some checks are performed to catch issues where the metadata and tree tips
#' don't match up. Any columns in `metadata` that are factors have all levels
#' that do not appear in the data dropped.
#'
#' @param tree An object convertible to a `tbl_graph`. This will usually be a
#'   "phylo" object, but see [tidygraph::tbl_graph] for more details.
#' @param metadata A data frame.
#' @param name Column in `metadata` that corresponds to the tree's tip labels
#'   (tidy-eval).
#' @param date Column in `metadata` that contains the date data (class "Date")
#'   for the tips (tidy-eval).
#'
#' @return An object of class "phylepic".
#' @export
#' @examples
#' library(ape)
#'
#' tree <- read.tree(system.file("enteric.newick", package = "phylepic"))
#' metadata <- read.csv(
#'   system.file("enteric_metadata.csv", package = "phylepic")
#' )
#' phylepic(tree, metadata, name, as.Date(collection_date))
phylepic <- function(tree, metadata, name, date) {
  if (!inherits(tree, get_s3_classes(tidygraph::as_tbl_graph))) {
    cli::cli_abort(c(
      "x" = "{.arg tree} must be convertible to a {.code tidygraph::tbl_graph}",
      "i" = "Often {.arg tree} will be a {.code phylo} object"
    ))
  }

  if (missing(name)) cli::cli_abort("{.arg name} is mandatory")
  if (missing(date)) cli::cli_abort("{.arg date} is mandatory")

  tips <-
    create_tree_layout(tree) |>
    dplyr::filter(.data$leaf) |>
    dplyr::transmute(.phylepic.index = .data$y, .phylepic.name = .data$name)

  tip_data <- dplyr::mutate(metadata, .phylepic.name = {{name}})
  if (!is.character(tip_data$.phylepic.name)) {
    cli::cli_abort(c(
      "!" = "The {.arg name} provided does not evaluate to a character vector",
      "x" = "In the metadata frame, {.code {rlang::as_label(rlang::enquo(name))}} has class {.cls {class(tip_data$.phylepic.name)}}"
    ))
  }

  tip_data <- tip_data |>
    dplyr::right_join(tips, by = ".phylepic.name") |>
    dplyr::mutate(.phylepic.date = {{date}})

  if (!inherits(tip_data$.phylepic.date, "Date")) {
    cli::cli_abort(c(
      "!" = "The {.arg date} provided does not evaluate to a Date vector",
      "x" = "In the metadata frame, {.code {rlang::as_label(rlang::enquo(date))}} has class {.cls {class(tip_data$.phylepic.date)}}"
    ))
  }

  class(tip_data) <- c("phylepic_tip_data", class(tip_data))

  tip_data <- dplyr::mutate(
    tip_data,
    dplyr::across(dplyr::where(is.factor), forcats::fct_drop)
  )

  structure(
    list(
      tree = tree,
      metadata = tip_data,
      dates = rlang::as_label(rlang::enquo(date))
    ),
    class = "phylepic"
  )
}

is.phylepic <- function(x) {
  inherits(x, "phylepic")
}

is.tip_data <- function(x) {
  inherits(x, "phylepic_tip_data")
}

#' @export
as.data.frame.phylepic <- function(x, ...) {
  x$metadata
}

#' @importFrom ape as.phylo
#' @export
as.phylo.phylepic <- function(x, ...) {
  as.phylo(x$tree, ...)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.phylepic <- function(x, ...) {
  tree <- deduplicate_nodes(x$tree)
  tbl_graph <- as_tbl_graph(tree, ...)
  salvage_node_labels(tree, tbl_graph)
}

salvage_node_labels <- function(tree, tbl_graph) {
  if (!is.null(attr(tree, "node.label.orig"))) {
    node_idx <- match(
      as.data.frame(tbl_graph, active = "nodes")$name,
      tree$node.label
    )
    orig_labels <- attr(tree, "node.label.orig")[node_idx]
    tbl_graph <- tbl_graph |>
      dplyr::mutate(node.label.orig = as.character(orig_labels))
  }
  tbl_graph
}

guess_bootstrap <- function(ggraph_layout) {
  source_col <- "node.label.orig"
  if (!"node.label.orig" %in% colnames(ggraph_layout)) source_col <- "name"
  values <- ggraph_layout[[source_col]][!ggraph_layout$leaf]
  if (all(grepl("^[0-9.]+/[0-9.]+$", values) | values == "")) {
    bootstrap <- ifelse(
      ggraph_layout$leaf,
      NA_character_,
      ggraph_layout[[source_col]]
    )
    bootstrap_numeric <- bootstrap |>
      strsplit("/") |>
      lapply(FUN = as.numeric) |>
      sapply(FUN = \(x) if (length(x) > 1) 100 * x[[1]] / x[[2]] else NA)
    ggraph_layout$.phylepic.bootstrap <- bootstrap
    ggraph_layout$.phylepic.bootstrap_numeric <- bootstrap_numeric
  }
  ggraph_layout
}

#' @export
print.phylepic <- function(x, ...) {
  phylo <- as.phylo(x)
  cat(sprintf(
    "Dated phylogenetic tree with %d tips, and their associated metadata.\n",
    ape::Ntip(phylo)
  ))
  cat("\nTip labels: ")
  utils::str(phylo$tip.label, give.head = FALSE, width = 70)
  cat(paste0("\nDate values: ", x$dates, "\n"))
  cat("\nMetadata ")
  meta <- dplyr::select(x$metadata, -dplyr::starts_with(".phylepic"))
  class(meta) <- "data.frame"
  utils::str(meta)
  invisible(NULL)
}

#' Create a graph layout for plotting
#'
#' This lays out a graph using `ggraph::create_layout()` with the `"dendrogram"`
#' layout, takes edge lengths from the tree, and flips the layout coordinates.
#' The plotting functions associated with [`phylepic()`] expect the graph to
#' be laid out using these settings.
#'
#' @param tree A tree-like graph or a `phylepic` object.
#' @param tip_data A data frame with tip metadata. There must be a column called
#'   `.phylepic.name` with values that correspond to the names of leaf nodes in
#'   the tree. If `NULL`, no tip data is joined onto the tree.
#'
#' @return A "layout_ggraph" object suitable for plotting with [ggplot2::ggplot]`.
#' @export
create_tree_layout <- function(tree, tip_data = NULL) {
  tree <- deduplicate_nodes(tree)
  tbl_graph <- as_tbl_graph(tree, directed = TRUE)
  tbl_graph <- salvage_node_labels(tree, tbl_graph)

  if (!is.null(tip_data)) {
    tbl_graph <- tbl_graph |>
      tidygraph::activate("nodes") |>
      dplyr::left_join(tip_data, by = c(name = ".phylepic.name"))
  }
  lo <- ggraph::create_layout(tbl_graph, layout = "dendrogram", length = length)
  lo <- guess_bootstrap(lo)
  flip_tree(lo)
}

flip_tree <- function(layout) {
  dplyr::rename(layout, x = .data$y, y = .data$x)
}

get_s3_classes <- function(f) {
  stopifnot(rlang::is_function(f))
  env <- rlang::get_env(f)
  path <- substitute(f)
  name <- if (deparse(path[[1]], nlines = 1L) %in% c("::", ":::")) {
    as.character(path[2:3])[[2]]
  } else {
    deparse(path)
  }

  methods_info <- utils::.S3methods(name, envir = env, dropPath = TRUE)
  methods_info <- attr(methods_info, "info")
  methods <- rownames(methods_info)
  substr(methods, nchar(name) + 2, nchar(methods))
}

deduplicate_nodes <- function(tree) {
  if (inherits(tree, "phylo") && any(duplicated(tree$node.label))) {
    orig_labels <- tree$node.label
    tree$node.label <- sprintf("node%05d", seq_along(tree$node.label))
    attr(tree, "node.label.orig") <- orig_labels
  }
  tree
}

#' Combine metadata (a line list) with a phylogenetic tree
#'
#' Some checks are performed to catch issues where the metadata and tree tips
#' don't match up. Any columns in `metadata` that are factors have all levels
#' that do not appear in the data dropped.
#'
#' To reduce surprises when matching `metadata` and `tree`, by default an error
#' occurs when there are tree tips that do not have associated metadata. On the
#' other hand, it it expected that `metadata` might contain rows that do not
#' correspond to the tips in `tree`.
#'
#' This often means that `factor` columns from `metadata` will contain levels
#' that do not appear at all in the tree. For plotting,
#' `ggplot2::discrete_scale` normally solves this with `drop = TRUE`, however
#' this can lead to inconsistencies when sharing the same scale across multiple
#' phylepic panels. `phylepic()` drops unused levels in all factors so that
#' scales can use `drop = FALSE` for consistency.
#'
#' @param tree An object convertible to a `tbl_graph`. This will usually be a
#'   "phylo" object, but see [tidygraph::tbl_graph] for more details.
#' @param metadata A data frame.
#' @param name Column in `metadata` that corresponds to the tree's tip labels
#'   (tidy-eval).
#' @param date Column in `metadata` that contains the date data (class "Date")
#'   for the tips (tidy-eval).
#' @param unmatched_tips Action to take when `tree` contains tip labels that
#'   do not appear in `name`. `"error"` aborts with an error message, `"drop"`
#'   drops unmatched tips from `tree`, `"keep"`.
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
phylepic <- function(
  tree,
  metadata,
  name,
  date,
  unmatched_tips = c("error", "drop", "keep")
) {
  if (!inherits(tree, get_s3_classes(tidygraph::as_tbl_graph))) {
    cli::cli_abort(c(
      "x" = "{.arg tree} must be convertible to a {.code tidygraph::tbl_graph}",
      "i" = "Often {.arg tree} will be a {.code phylo} object"
    ))
  }

  if (missing(name)) cli::cli_abort("{.arg name} is mandatory")
  if (missing(date)) cli::cli_abort("{.arg date} is mandatory")

  tips <- minimal_tip_data_frame(tree)

  tip_data <- dplyr::mutate(metadata, .phylepic.name = {{name}})
  if (!is.character(tip_data$.phylepic.name)) {
    cli::cli_abort(c(
      "!" = "The {.arg name} provided does not evaluate to a character vector",
      "x" = "In {.arg metadata}, {.code {rlang::as_label(rlang::enquo(name))}} has class {.cls {class(tip_data$.phylepic.name)}}"
    ))
  }

  extra_tips <- setdiff(tips$.phylepic.name, tip_data$.phylepic.name)
  if (length(extra_tips) > 0) {
    unmatched_tips <- match.arg(unmatched_tips)
    if (unmatched_tips == "error") {
      cli::cli_abort(c(
        "!" = "{.arg tree} contains tips that do not appear in {.arg metadata}",
        "x" = "Unmatched tips: {extra_tips}",
        "i" = 'To fix this, try {.code unmatched_tips = "drop"} or {.code unmatched_tips = "keep"}'
      ))
    } else if (unmatched_tips == "drop") {
      tree <- subtree(tree, intersect(tips$.phylepic.name, tip_data$.phylepic.name))
      n_dropped <- nrow(tips) - sum(is_leaf(tree))
      tips <- minimal_tip_data_frame(tree)
      cli::cli_inform("Dropped {n_dropped} unmatched tips from tree, as requested")
    } else if (unmatched_tips == "keep") {
      # nothing to do
    }
  }

  tip_data <- tip_data |>
    dplyr::right_join(tips, by = ".phylepic.name", relationship = "one-to-one") |>
    dplyr::mutate(.phylepic.date = {{date}})

  if (!inherits(tip_data$.phylepic.date, "Date")) {
    cli::cli_abort(c(
      "!" = "The {.arg date} provided does not evaluate to a Date vector",
      "x" = "In {.arg metadata}, {.code {rlang::as_label(rlang::enquo(date))}} has class {.cls {class(tip_data$.phylepic.date)}}"
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

subtree <- function(tree, tip) {
  tree <- deduplicate_nodes(tree)
  tbl_graph <- as_tbl_graph(tree, directed = TRUE)
  tbl_graph <- salvage_node_labels(tree, tbl_graph)

  repeat {
    retain <- union(
      match(tip, igraph::V(tbl_graph)$name),
      which(!is_leaf(tbl_graph))
    )
    if (length(retain) == igraph::vcount(tbl_graph)) break
    tbl_graph <- igraph::induced_subgraph(tbl_graph, retain)
  }

  as_tbl_graph(tbl_graph, directed = TRUE)
}

is_leaf <- function(graph) {
  if (igraph::is_directed(graph)) {
    deg_in <- igraph::degree(graph, mode = "in") == 0
    deg_out <- igraph::degree(graph, mode = "out") == 0
    if (sum(deg_out) > sum(deg_in)) deg_out else deg_in
  } else {
    igraph::degree(graph, mode = "all") == 1
  }
}

minimal_tip_data_frame <- function(tree) {
  create_tree_layout(tree) |>
    dplyr::filter(.data$leaf) |>
    dplyr::transmute(.phylepic.index = .data$y, .phylepic.name = .data$name)
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

  methods_info <- utils::.S3methods(name, envir = env)
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

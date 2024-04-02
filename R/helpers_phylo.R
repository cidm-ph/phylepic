#' Drop a clade from a phylogentic tree
#'
#' `drop.clade` invokes [ape::drop.tip()] on all tips descendent from the
#' specified node. This is convenient when used alongside [ape::getMRCA()] to
#' drop a clade defined by the most recent common ancestor of a set of tips,
#' rather than exhaustively specifying all of its tips.
#'
#' @param phy an object of class "phylo".
#' @param node number specifying the parent node of the clade to delete.
#' @param root.edge,collapse.singles passed to [ape::drop.tip()].
#'
#' @return New phylo object with the chosen clade removed
#' @export
#'
#' @examples
#' library("ape")
#' data(bird.orders)
#' plot(bird.orders)
#'
#' # find the common ancestor of some tips
#' mrca <- ape::getMRCA(bird.orders, c("Passeriformes", "Coliiformes"))
#'
#' # drop the clade descending from that ancestor
#' plot(drop.clade(bird.orders, mrca))
drop.clade <- function(phy, node, root.edge = 0, collapse.singles = TRUE) {
  stopifnot(is.integer(node))

  n <- ape::Ntip(phy)
  if (node == n + 1L)  return(phy)
  ape::drop.tip(
    phy,
    ape::prop.part(phy)[[node - n]],
    root.edge = root.edge,
    collapse.singles = collapse.singles
  )
}

##' @export
unmask_linked <- function(x, linkage_graph, linked_only = FALSE) {
  UseMethod("unmask_linked", x)
}

##' @export
unmask_linked.varkit <- function(x, linkage_graph, linked_only = FALSE) {
  validate_varkit(x)
  stopifnot(is_varkit_linkage_graph(linkage_graph))
  .stopifnotsingle(linked_only, what = "logical")
  .ensure_igraph()

  unmask_idx <- as_integer(igraph::as_edgelist(linkage_graph, names = TRUE))

  use_site <- .indices(x)$sites
  if (max(unmask_idx) > length(use_site))
    stop("The 'linkage_graph' references sites not in 'x'!")

  unmask_idx <- unmask_idx[unmask_idx[, 1L, drop = TRUE] %in% which(use_site), 2L, drop = TRUE]

  if (linked_only)
    use_site <- rep(FALSE, length(use_site))

  use_site[unmask_idx] <- TRUE
  .indices(x)$sites <- use_site

  x
}

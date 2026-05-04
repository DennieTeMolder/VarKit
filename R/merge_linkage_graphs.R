##' @export
merge_linkage_graphs <- function(child, parent) {
  stopifnot(is_varkit_linkage_graph(child))
  stopifnot(is_varkit_linkage_graph(parent))

  # Combine origins to create a lineage
  origin <- c(get_origin(child), get_origin(parent))
  origin <- origin[!duplicated(origin, fromLast = TRUE)]

  # Convert graphs to a quickly searchable format
  # NOTE: this code should be similar to groups.varkit_linkage_graph()
  parent <- igraph::as_edgelist(parent, names = TRUE)

  # None of the CHILD nodes should have in-coming edges in PARENT
  if (any(names(igraph::V(child)) %in% parent[, 2L, drop = TRUE]))
    stop("Merge conflict! Are 'child' and 'parent' swapped?")

  parent <- split(parent[, 2L, drop = TRUE], f = parent[, 1L, drop = TRUE])
  child <- igraph::as_edgelist(child, names = TRUE)
  child <- split(child[, 2L, drop = TRUE], f = child[, 1L, drop = TRUE])

  # Merge overlapping elements of PARENT into CHILD
  visited <- vector("list", length = length(child))
  for (i in seq_along(child)) {
    group <- child[[i]]
    iname <- names(child)[i]
    visited[[i]] <- c(iname, group)
    child[[i]] <- c(group, .unlist(parent[visited[[i]]]))
  }

  # Merge non-overlapping elements into CHILD
  visited <- .unlist(visited)
  parent[visited] <- NULL
  child[names(parent)] <- parent

  # Covert merged edges back to a linkage graph
  child <- purrr::imap(child, function(to, from) cbind(from, to))
  child <- do.call(rbind, child)
  varkit_linkage_graph(igraph::graph_from_edgelist(child), origin = origin)
}

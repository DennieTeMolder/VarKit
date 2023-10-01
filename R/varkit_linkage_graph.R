##' @export
varkit_linkage_graph <- function(graph, origin = NULL) {
  stopifnot(igraph::is_dag(graph))
  stopifnot(igraph::is_named(graph))
  stopifnot(igraph::vcount(graph) > igraph::ecount(graph))

  if (!is.null(origin)) {
    graph <- set_origin(graph, origin)
  }

  class(graph) <- c("varkit_linkage_graph", class(graph))
  graph
}

##' @export
summary.varkit_linkage_graph <- function(object, ..., vcf = NULL) {
  if (!is.null(vcf))
    stopifnot(is_varkit(vcf), originates_from(object, origin = vcf))

  tbl <- tibble::tibble(
    n_remove = sum(igraph::degree(object, mode = "in") == 1),
    n_proxy = sum(igraph::degree(object, mode = "out") > 0),
    n_singletons = igraph::vcount(object) - .data$n_proxy - .data$n_remove
  )

  if (!is.null(vcf)) {
    tbl$n_total <- sum(vcf$use_site)
    tbl$frac_remove <- tbl$n_remove / tbl$n_total
  }

  tbl
}

##' @exportS3Method igraph::groups
groups.varkit_linkage_graph <- function(x) {
  nodes <- igraph::degree(x, mode = "out")
  nodes <- names(nodes)[nodes > 0]
  sapply(nodes, function(node) {
    igraph::neighbors(x, v = node, mode = "out")$name
  }, simplify = FALSE)
}

##' @export
varkit_linkage_graph <- function(graph, origin = NULL) {
  .ensure_igraph()
  stopifnot(igraph::is_dag(graph))
  stopifnot(igraph::is_named(graph))
  stopifnot(igraph::vcount(graph) > igraph::ecount(graph))

  if (!is.null(origin)) {
    graph <- set_origin(graph, origin)
  }

  class(graph) <- unique(c("varkit_linkage_graph", class(graph)))
  graph
}

##' @export
is_varkit_linkage_graph <- function(x) {
  inherits(x, "varkit_linkage_graph")
}

##' @export
summary.varkit_linkage_graph <- function(object, vcf = NULL, ...) {
  .ensure_igraph()
  if (!is.null(vcf))
    stopifnot(is_varkit(vcf), originates_from(object, origin = vcf))

  tbl <- tibble::tibble(
    n_remove = sum(igraph::degree(object, mode = "in") == 1),
    n_proxy = sum(igraph::degree(object, mode = "out") > 0),
  )

  if (!is.null(vcf)) {
    tbl$n_singletons <- n_sites.varkit(vcf) - tbl$n_proxy - tbl$n_remove
    tbl$n_total <- with(tbl, n_remove + n_proxy + n_singletons)
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

# NOTE using saveRDS() also seems to work and uses 10x less disk space (binary format).
##' @export
write_linkage_graph <- function(graph, file, format = "xml") {
  .ensure_igraph()
  if (!format[1] %in% c("xml", "graphml"))
    stop("Only the 'xml' (graphml) format is supported! Others lack graph and/or vertex attributes.")
  igraph::write_graph(graph, file = file, format = "graphml")
}

##' @export
read_linkage_graph <- function(file) {
  .ensure_igraph()
  graph <- igraph::read_graph(file, format = "graphml")
  igraph::remove.vertex.attribute(graph, "id")
}

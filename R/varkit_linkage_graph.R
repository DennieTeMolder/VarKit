##' @export
varkit_linkage_graph <- function(graph, origin = NULL) {
  .ensure_igraph()
  stopifnot(igraph::is_dag(graph))
  stopifnot(igraph::is_named(graph))
  stopifnot(igraph::vcount(graph) > igraph::ecount(graph))

  if (!is.null(origin)) {
    stopifnot(is.character(origin), is.vector(origin))
    igraph::graph_attr(graph, "varkit_origin") <- origin
  }

  class(graph) <- union("varkit_linkage_graph", class(graph))
  graph
}

##' @export
is_varkit_linkage_graph <- function(x) {
  inherits(x, "varkit_linkage_graph")
}

##' @export
summary.varkit_linkage_graph <- function(object, ...) {
  rlang::check_dots_used()
  .ensure_igraph()

  tibble::tibble(
    n_proxy = sum(igraph::degree(object, mode = "out") > 0),
    n_remove = sum(igraph::degree(object, mode = "in") == 1)
  )
}

##' @exportS3Method igraph::groups
groups.varkit_linkage_graph <- function(x) {
  res <- igraph::as_edgelist(x, names = TRUE)
  res <- split(res[, 2L, drop = TRUE], f = res[, 1L, drop = TRUE])
  res[order(as.integer(names(res)))]
}

# NOTE using saveRDS() also seems to work and uses 10x less disk space (binary format).
##' @export
write_linkage_graph <- function(graph, file, format = "xml") {
  .stopifnotsingle(format, what = "character")
  .ensure_igraph()

  if (!format %in% c("xml", "graphml"))
    stop("Only the 'xml' (graphml) format is supported! Others lack graph and/or vertex attributes.")
  igraph::write_graph(graph, file = file, format = "graphml")
}

##' @export
read_linkage_graph <- function(file) {
  .ensure_igraph()
  graph <- igraph::read_graph(file, format = "graphml")
  graph <- igraph::delete_vertex_attr(graph, "id")
  varkit_linkage_graph(graph)
}

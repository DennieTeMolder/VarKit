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
  rlang::check_dots_used()
  .ensure_igraph()
  if (!is.null(vcf))
    stopifnot(is_varkit(vcf), originates_from(object, origin = vcf))

  tibble::tibble(
    n_proxy = sum(igraph::degree(object, mode = "out") > 0),
    n_remove = sum(igraph::degree(object, mode = "in") == 1),
    n_total = if (!is.null(vcf)) n_sites.varkit(vcf, mask = FALSE),
    frac_remove = if (!is.null(vcf)) .data$n_remove / .data$n_total
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
  .ensure_igraph()
  if (!format[1] %in% c("xml", "graphml"))
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

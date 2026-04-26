##' @export
is_linked <- function(vcf, linkage_graph) {
  validate_varkit(vcf)
  stopifnot(is_varkit_linkage_graph(linkage_graph))
  .ensure_igraph()

  if (!originates_from(linkage_graph, origin = vcf))
    stop("The 'linkage_graph' does not originate from 'vcf'!")

  deg <- igraph::degree(linkage_graph, mode = "in")
  if (any(deg >= 2L))
    stop("The 'linkage_graph' appears to be corrupted!")

  is_linked <- rep(FALSE, n_sites.varkit(vcf, mask = FALSE))
  is_linked[as.integer(names(which(deg == 1)))] <- TRUE
  is_linked[.indices(vcf)$sites]
}

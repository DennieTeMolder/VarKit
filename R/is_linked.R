##' @export
is_linked <- function(vcf, linkage_graph, check = TRUE) {
  validate_varkit(vcf)
  stopifnot(is_varkit_linkage_graph(linkage_graph))
  .stopifnotsingle(check, what = "logical")
  .ensure_igraph()

  if (check && !originates_from(linkage_graph, origin = vcf))
    stop("The 'linkage_graph' does not originate from 'vcf'!")

  deg <- igraph::degree(linkage_graph, mode = "in")
  if (any(deg >= 2L))
    stop("The 'linkage_graph' appears to be corrupted!")

  site_ind <- .indices(vcf)$sites
  is_linked <- rep(FALSE, length(site_ind))
  is_linked[as.integer(names(deg)[deg == 1])] <- TRUE
  is_linked[site_ind]
}

##' @export
collapse_alt <- function(vcf, alt_idx) {
  stopifnot(is_varkit(vcf))
  if (!"alt" %in% colnames(vcf$site))
    stop("The 'alt' column is missing from site(vcf)!")
  stopifnot(is.numeric(alt_idx), is.vector(alt_idx))
  if (length(alt_idx) != n_sites.varkit(vcf, mask = FALSE)) {
    if (length(alt_idx) == n_sites.varkit(vcf, mask = TRUE))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The length of 'alt_idx' does not match n_sites(vcf)!")
  }

  alt <- site.varkit(vcf, alt)
  alt <- stringr::str_split_fixed(alt, stringr::fixed(","), n = max(alt_idx) + 1L)
  alt <- matrixStats::rowCollapse(alt, alt_idx)
  vcf$site$alt <- alt

  vcf
}

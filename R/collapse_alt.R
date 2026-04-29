##' @export
collapse_alt <- function(vcf, alt_idx) {
  validate_varkit(vcf)
  stopifnot(is.numeric(alt_idx), is.vector(alt_idx))
  if (length(alt_idx) != 1L && length(alt_idx) != n_sites.varkit(vcf, mask = FALSE)) {
    if (length(alt_idx) == n_sites.varkit(vcf, mask = TRUE))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The length of 'alt_idx' does not match n_sites(vcf)!")
  }

  alt <- rd_unclass(vcf)$site$alt
  if (is.null(alt))
    stop("The '$site$alt' column is missing!")

  n_allele <- stringr::str_count(alt, pattern = stringr::fixed(",")) + 1L
  if (any(alt_idx > n_allele, na.rm = TRUE))
    stop("Values of 'alt_idx' are out of range!")
  if (all(n_allele == 1L, na.rm = TRUE))
    return(vcf)

  alt <- stringr::str_split_fixed(
    alt,
    pattern = stringr::fixed(","),
    n = max(n_allele, na.rm = TRUE)
  )
  alt <- matrixStats::rowCollapse(alt, idxs = alt_idx)

  vcf$site$alt <- alt
  vcf
}

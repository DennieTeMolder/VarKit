##' @export
varkit_filter <- function(geno, site = NULL, sample = NULL) {
  UseMethod("varkit_filter", geno)
}

##' @export
varkit_filter.varkit_geno <- function(geno, site = NULL, sample = NULL) {
  # Checks
  if (!is.null(site)) {
    stopifnot(is.logical(site))
    stopifnot(length(site) == ncol(geno))
  }
  if (!is.null(sample)) {
    stopifnot(is.logical(sample) )
    stopifnot(length(sample) == nrow(geno))
  }

  # Skip if all sites/samples are asked
  if (all(site))
    site <- NULL
  if (all(sample))
    sample <- NULL

  # Return requested selection
  if (!is.null(site) && !is.null(sample)) {
    return(geno[sample, site])
  } else if (!is.null(site)) {
    return(geno[, site])
  } else if (!is.null(sample)) {
    return(geno[sample, ])
  }

  # No selection, return full genotype matrix
  geno
}

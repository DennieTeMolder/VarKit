##' @export
varkit_filter <- function(geno, site = NULL, sample = NULL) {
  UseMethod("varkit_filter", geno)
}

##' @export
varkit_filter.varkit_geno <- function(geno, site = NULL, sample = NULL) {
  # Checks
  if (is.null(site) && is.null(sample))
    stop("Please provide the 'site' and/or 'sample' argument.")
  if (!is.null(site))
    stopifnot(is.logical(site) && length(site) == ncol(geno))
  if (!is.null(sample))
    stopifnot(is.logical(sample) && length(sample) == nrow(geno))

  # Single selection argument provided
  if (is.null(site))
    return(geno[sample, ])
  if (is.null(sample))
    return(geno[, site])

  # Both selection arguments provided
  geno[sample, site]
}

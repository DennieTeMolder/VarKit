##' @export
get_idx <- function(geno, site = NULL, sample = NULL) {
  UseMethod("get_idx", geno)
}

##' @export
get_idx.varkit_geno <- function(geno, site = NULL, sample = NULL) {
  # Checks
  if (is.null(site) && is.null(sample))
    stop("Please provide the 'site' and/or 'sample' argument.")
  if (!is.null(site))
    stopifnot(is.integer(site))
  if (!is.null(sample))
    stopifnot(is.integer(sample))

  # Single selection argument provided
  if (is.null(site))
    return(geno[sample, ])
  if (is.null(sample))
    return(geno[, site])

  # Both selection arguments provided
  geno[sample, site]
}

##' @export
mask <- function(x, site = NULL, sample = NULL) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site = NULL, sample = NULL) {
  # Check
  if (is.null(site) && is.null(sample))
    stop("Please provide the 'site' and/or 'sample' argument.")
  # Mask
  if (!is.null(site)) {
    idx <- get_site_idx(site, current_sites = sites(x, add_index = TRUE))
    x$use_site[idx] <- FALSE
  }
  if (!is.null(sample)) {
    idx <- get_sample_idx(sample, all_samples = x$samples, use_sample = x$use_sample)
    x$use_sample[idx] <- FALSE
  }
  x
}

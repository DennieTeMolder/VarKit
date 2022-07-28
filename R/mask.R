##' @export
mask <- function(x, site = NULL, sample = NULL) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site = NULL, sample = NULL) {
  # Check
  if (is.null(site) && is.null(sample))
    stop("Please provide the 'site' and/or 'sample' argument.")
  if (!is.null(site))
    stopifnot(is.integer(site) || is.data.frame(site))
  if (!is.null(sample))
    stopifnot(is.integer(sample) || is.character(sample))

  # Convert input
  if (!is.null(site) && is.data.frame(site)) {
    stopifnot(colnames(site) == c("chrom", "pos"))
    all <- dplyr::mutate(sites(x), idx = seq_along(x$use_sites))
    site <- dplyr::inner_join(site, all, by = c("chrom", "pos"))$idx
  }
  if (!is.null(sample) && is.character(sample))
    sample <- which(sample %in% samples(x))

  # Perform masking
  if (!is.null(site))
    x$use_sites <- x$use_sites[-site]
  if (!is.null(sample))
    x$use_samples <- x$use_samples[-sample]

  x
}

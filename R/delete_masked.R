##' @export
delete_masked <- function(x, samples = TRUE, sites = TRUE) {
  UseMethod("delete_masked", x)
}

##' @export
delete_masked.varkit <- function(x, samples = TRUE, sites = TRUE) {
  validate_varkit(x)
  .stopifnotsingle(samples, what = "logical")
  .stopifnotsingle(sites, what = "logical")

  # Check if the mask is active, else disable
  if (rlang::is_missing(drm_which(x, "samples")))
    samples <- FALSE
  if (rlang::is_missing(drm_which(x, "sites")))
    sites <- FALSE

  if (!(samples || sites))
    return(x)

  # Create a copy of the data relation model, with a reduced number of sites/samples
  drmodel <- drm_copy(drm_get(x))
  if (samples)
    drm_delete(drmodel, "samples")
  if (sites)
    drm_delete(drmodel, "sites")

  # A value of NULL will use the stored mask, a value of TRUE will select all
  samples <- if (samples) NULL else TRUE
  sites <- if (sites) NULL else TRUE

  new_varkit(
    meta = x$meta,
    site = drm_do_subset(x$site, sites = sites),
    geno = drm_do_subset(x$geno, samples = samples, sites = sites),
    drmodel = drmodel
  )
}

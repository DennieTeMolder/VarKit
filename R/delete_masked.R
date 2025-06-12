##' @export
delete_masked <- function(x, samples = TRUE, sites = TRUE) {
  UseMethod("delete_masked", x)
}

##' @export
delete_masked.varkit <- function(x, samples = TRUE, sites = TRUE) {
  validate_varkit(x)
  .stopifnotsingle(samples, what = "logical")
  .stopifnotsingle(sites, what = "logical")
  if (!(samples || sites))
    stop("Either 'samples' or 'sites' must be TRUE!")

  # Create a copy of the data relation model, with a reduced number of sites/samples
  drmodel <- drm_copy(drm_get(x))
  if (samples)
    drm_delete(drmodel, "samples")
  if (sites)
    drm_delete(drmodel, "sites")

  samples <- if (samples) drm_which(x, "samples") else rlang::missing_arg()
  sites <- if (sites) drm_which(x, "sites") else rlang::missing_arg()

  new_varkit(
    meta = x$meta,
    site = x$site[sites, , drop = FALSE],
    geno = x$geno[samples, sites, drop = FALSE],
    drmodel = drmodel
  )
}

##' @export
import_mask <- function(x, mask, add = FALSE) {
  UseMethod("import_mask", x)
}

##' @export
import_mask.varkit <- function(x, mask, add = FALSE) {
  validate_varkit(x)
  stopifnot(is.list(mask), c("samples", "sites") %in% names(mask))
  stopifnot(is.logical(mask$samples), is.logical(mask$sites))
  .stopifnotsingle(add, what = "logical")

  if (add) {
    mask$samples <- mask$samples & drm_ifelse(x, node = "samples")
    mask$sites <- mask$sites & drm_ifelse(x, node = "sites")
  }

  # Apply mask
  drm_preselect(x, samples = mask$samples, sites = mask$sites)
}

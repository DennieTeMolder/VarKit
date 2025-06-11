##' @export
n_samples <- function(x, ...) {
  UseMethod("n_samples", x)
}

##' @export
n_samples.varkit <- function(x, ..., mask = TRUE) {
  .stopifnotsingle(mask, what = "logical")
  if (mask) {
    drm_n_selected(drm_get(x$geno), "samples")
  } else {
    drm_n_total(drm_get(x$geno), "samples")
  }
}

##' @export
n_samples.varkit_header <- function(x, ...) {
  length(samples.varkit_header(x))
}

##' @export
n_samples.varkit_geno <- function(x, ...) {
  nrow(x)
}

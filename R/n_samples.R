##' @export
n_samples <- function(x, ...) {
  UseMethod("n_samples", x)
}

##' @export
n_samples.varkit <- function(x, ..., mask = TRUE) {
  .stopifnotsingle(mask, what = "logical")
  if (mask) {
    sum(.indices(x)$samples)
  } else {
    length(.indices(x)$samples)
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

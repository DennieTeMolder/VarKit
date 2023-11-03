##' @export
n_samples <- function(x) {
  UseMethod("n_samples", x)
}

##' @export
n_samples.varkit <- function(x) {
  length(samples(x))
}

##' @export
n_samples.varkit_header <- n_samples.varkit

##' @export
n_samples.varkit_geno <- function(x) {
  nrow(x)
}

##' @export
samples <- function(x) {
  UseMethod("samples", x)
}

##' @export
samples.varkit <- function(x) {
  x$samples[x$use_sample]
}

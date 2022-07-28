##' @export
samples <- function(x) {
  UseMethod("samples", x)
}

##' @export
samples.varkit_geno <- function(x) {
  rownames(x)
}

##' @export
samples.varkit <- function(x) {
  samples(x$geno)[x$use_samples]
}

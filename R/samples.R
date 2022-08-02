##' @export
samples <- function(x) {
  UseMethod("samples", x)
}

##' @export
samples.varkit <- function(x) {
  rownames(x$geno)
}

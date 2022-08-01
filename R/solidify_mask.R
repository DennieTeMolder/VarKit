##' @export
solidify_mask <- function(x) {
  UseMethod("solidify_mask", x)
}

##' @export
solidify_mask.varkit <- function(x) {
  x$fixed = fixed(x)
  x$geno = geno(x)
  x$samples = samples(x)
  x$use_site = rep(TRUE, ncol(x$geno))
  x
}

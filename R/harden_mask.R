##' @export
harden_mask <- function(x) {
  UseMethod("solidify_mask", x)
}

##' @export
harden_mask.varkit <- function(x) {
  x$fixed = fixed(x)
  x$geno = geno(x)
  x$samples = samples(x)
  x$use_site = rep(TRUE, ncol(x$geno))
  x
}

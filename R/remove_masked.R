##' @export
remove_masked <- function(x) {
  UseMethod("remove_masked", x)
}

##' @export
remove_masked.varkit <- function(x) {
  x$fixed <- fixed(x)
  x$geno <- geno(x)
  x$samples <- samples(x)
  x$use_site <- rep(TRUE, ncol(x$geno))
  x
}

##' @export
remove_masked <- function(x) {
  UseMethod("remove_masked", x)
}

##' @export
remove_masked.varkit <- function(x) {
  x$site <- site(x)
  x$geno <- geno(x)
  x$use_site <- rep(TRUE, ncol(x$geno))
  x
}

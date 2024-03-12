##' @export
delete_masked_sites <- function(x) {
  UseMethod("delete_masked_sites", x)
}

##' @export
delete_masked_sites.varkit <- function(x) {
  x$site <- site(x)
  x$geno <- geno(x)
  x$use_site <- rep(TRUE, n_sites.varkit_geno(x$geno))
  x
}

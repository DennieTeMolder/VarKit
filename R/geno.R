##' @export
geno <- function(x) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x) {
  varkit_filter(x$geno, site = x$use_site, sample = x$use_sample)
}

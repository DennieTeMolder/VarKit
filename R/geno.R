##' @export
geno <- function(x) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x) {
  get_idx(x$geno, site = x$use_sites, sample = x$use_samples)
}

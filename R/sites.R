##' @export
sites <- function(x) {
  UseMethod("sites", x)
}

##' @export
sites.varkit <- function(x) {
  dplyr::select(fixed(x), chrom, pos)
}

##' @export
sites.varkit_geno <- function(x) {
  1:ncol(x)
}

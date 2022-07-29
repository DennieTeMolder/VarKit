##' @export
sites <- function(x) {
  UseMethod("sites", x)
}

##' @export
sites.varkit <- function(x) {
  dplyr::select(fixed(x), chrom, pos)
}

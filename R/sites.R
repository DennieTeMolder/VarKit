##' @export
sites <- function(x, add_index = FALSE) {
  UseMethod("sites", x)
}

##' @export
sites.varkit <- function(x, add_index = FALSE) {
  sites <- dplyr::select(fixed(x), chrom, pos)
  if (add_index)
    sites <- dplyr::bind_cols(sites, idx = which(x$use_site))
  sites
}

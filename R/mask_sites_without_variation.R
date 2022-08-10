##' @export
mask_sites_without_variation <- function(x) {
  UseMethod("mask_sites_without_variation", x)
}

##' @export
mask_sites_without_variation.varkit <- function(x) {
  is_not_variable <- apply(geno(x), 2, .not_variable)
  mask(x, site = is_not_variable)
}

.not_variable <- function(x) {
  stopifnot(is.vector(x))
  x <- x[!is.na(x)]
  if (length(x) <= 1)
    return(TRUE)
  all(duplicated(x)[-1])
}

##' @export
mask_sites_without_variation <- function(x) {
  UseMethod("mask_sites_without_variation", x)
}

##' @export
mask_sites_without_variation.varkit <- function(x) {
  is_not_variable <- apply_sites(geno(x), .not_variable)
  mask(x, site = is_not_variable)
}

.not_variable <- function(x, na_rm = TRUE) {
  stopifnot(is.vector(x))
  if (na_rm)
    x <- x[!is.na(x)]
  if (length(x) <= 1)
    return(TRUE)
  all(duplicated(x)[-1])
}

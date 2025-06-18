##' @export
mask_sites_without_variation <- function(x, parse_geno_type = NULL) {
  UseMethod("mask_sites_without_variation", x)
}

##' @export
mask_sites_without_variation.varkit <- function(x, parse_geno_type = NULL) {
  if (is.null(parse_geno_type)) {
    mat <- geno.varkit(x)
  } else {
    mat <- parse_geno.varkit(copy_varkit(x), type = parse_geno_type)
  }

  mask(x, site = has_no_variation(mat, margin = 2, na.rm = TRUE))
}

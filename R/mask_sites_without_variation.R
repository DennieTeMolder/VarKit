##' @export
mask_sites_without_variation <- function(x, parse_geno_type = NULL) {
  UseMethod("mask_sites_without_variation", x)
}

##' @export
mask_sites_without_variation.varkit <- function(x, parse_geno_type = NULL) {
  if (!is.null(parse_geno_type)) {
    data <- parse_geno.varkit(x, type = parse_geno_type)
  } else {
    data <- geno(x)
  }
  is_not_variable <- apply_sites(data, .not_variable)
  mask(x, site = is_not_variable)
}

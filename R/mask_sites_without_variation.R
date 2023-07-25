##' @export
mask_sites_without_variation <- function(x, format_type = NULL) {
  UseMethod("mask_sites_without_variation", x)
}

##' @export
mask_sites_without_variation.varkit <- function(x, format_type = NULL) {
  if (!is.null(format_type)) {
    data <- parse_format_type.varkit(x, type = format_type)
  } else {
    data <- geno(x)
  }
  is_not_variable <- apply_sites(data, .not_variable)
  mask(x, site = is_not_variable)
}

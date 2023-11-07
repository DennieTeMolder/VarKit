##' @export
mask_sites_without_variation <- function(x, parse_geno_type = NULL) {
  UseMethod("mask_sites_without_variation", x)
}

##' @export
mask_sites_without_variation.varkit <- function(x, parse_geno_type = NULL) {
  if (is.null(parse_geno_type)) {
    data <- x$geno
    use <- if (all(x$use_site)) NULL else x$use_site
  } else {
    data <- parse_geno.varkit(x, type = parse_geno_type)
    use <- NULL
  }

  mask(x, site = !has_variation.matrix(data, margin = 2, use = use))
}

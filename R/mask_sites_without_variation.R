##' @export
mask_sites_without_variation <- function(x, parse_geno_type = NULL, why = NULL, na_exclude = TRUE) {
  UseMethod("mask_sites_without_variation", x)
}

##' @export
mask_sites_without_variation.varkit <- function(x, parse_geno_type = NULL, why = NULL, na_exclude = TRUE) {
  if (is.null(parse_geno_type)) {
    mat <- geno.varkit(x)
  } else {
    mat <- parse_geno.varkit(x, type = parse_geno_type)
  }

  mask.varkit(x,
    site = has_no_variation(mat, margin = 2, na_exclude = na_exclude),
    why = why,
    na_mask = FALSE
  )
}

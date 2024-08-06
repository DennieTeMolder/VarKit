##' @export
mask <- function(x, site, why = NULL, na_mask = TRUE) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site, why = NULL, na_mask = TRUE) {
  if (!is.null(why))
    .stopifnotsingle(why, what = "character")

  # Mask
  idx <- get_site_idx(site, vcf = x, na_get = na_mask)
  x$use_site[idx] <- FALSE

  if (!any(x$use_site))
    stop("The requested mask will hide all sites!")

  if (!is.null(why)) {
    if (!"filter" %in% colnames(x$site))
      x$site$filter <- ifelse(x$use_site, "PASS", "MASKED")
    x$site$filter[idx] <- why
  }
  x
}

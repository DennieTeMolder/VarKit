##' @export
mask <- function(x, site, why = NULL) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site, why = NULL) {
  if (!is.null(why))
    .stopifnotsingle(why, class = "character")

  # Mask
  idx <- get_site_idx(site, current_sites = fixed(x, add_index = TRUE, only_positions = TRUE))
  x$use_site[idx] <- FALSE

  if (!any(x$use_site))
    stop("The requested mask will hide all sites!")

  if (!is.null(why)) {
    if (!"filter" %in% colnames(x$fixed))
      x$fixed$filter <- ifelse(x$use_site, "PASS", "MASKED")
    x$fixed$filter[idx] <- why
  }
  x
}

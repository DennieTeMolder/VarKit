##' @export
mask <- function(x, site, recode_filter = NULL) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site, recode_filter = NULL) {
  if (!is.null(recode_filter))
    stopifnot(is.vector(recode_filter), length(recode_filter) == 1)

  # Mask
  idx <- get_site_idx(site, current_sites = fixed(x, add_index = TRUE, only_positions = TRUE))
  x$use_site[idx] <- FALSE

  if (!any(x$use_site))
    stop("The requested mask will hide all sites!")

  if (!is.null(recode_filter)) {
    if (!"filter" %in% colnames(x$fixed))
      x$fixed$filter <- ifelse(x$use_site, "PASS", "MASKED")
    x$fixed$filter[idx] <- recode_filter
  }
  x
}

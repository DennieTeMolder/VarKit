##' @export
mask <- function(x, site) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site) {
  # Mask
  idx <- get_site_idx(site, current_sites = fixed(x, add_index = TRUE, only_positions = TRUE))
  x$use_site[idx] <- FALSE
  if (!any(x$use_site))
    stop("The requested mask will hide all sites!")
  x
}

##' @export
retain <- function(x, site, why = NULL) {
  UseMethod("retain", x)
}

##' @export
retain.varkit <- function(x, site, why = NULL) {
  if (!is.null(why))
    .stopifnotsingle(why, class = "character")

  idx <- get_site_idx(site, x)
  if (length(idx) < 1)
    stop("No sites remaining! Check your input.")

  # Mask non-matching
  x$use_site <- rep(FALSE, length(x$use_site))
  x$use_site[idx] <- TRUE

  if (!is.null(why)) {
    if (!"filter" %in% colnames(x$site))
      x$site$filter <- ifelse(x$use_site, "PASS", "MASKED")
    x$site$filter[idx] <- why
  }
  x
}

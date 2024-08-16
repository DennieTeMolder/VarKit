##' @export
retain <- function(x, site, why = NULL, na_retain = TRUE) {
  UseMethod("retain", x)
}

##' @export
retain.varkit <- function(x, site, why = NULL, na_retain = TRUE) {
  if (!is.null(why))
    .stopifnotsingle(why, what = "character")
  if (is_varkit_linkage_graph(site))
    stop("Linkage graphs only support the mask() function!")

  # dplyr::filter() like argument evaluation (supports: ., site, geno, meta, names(site))
  site <- with_vcf(x, expr = {{ site }})

  idx <- get_site_idx(site, vcf = x, na_get = na_retain)
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

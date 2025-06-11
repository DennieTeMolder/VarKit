##' @export
mask <- function(x, site, why = NULL, na_mask = TRUE, ...) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site, why = NULL, na_mask = TRUE, ...) {
  if (!is.null(why))
    .stopifnotsingle(why, what = "character")

  # dplyr::filter() like argument evaluation (supports: ., site, geno, meta, names(site))
  site <- with_vcf(x, expr = {{ site }})

  if (is.data.frame(site))
    lifecycle::deprecate_stop("v0.9.0", "mask(site = 'should be boolean')", I("mask(site = has_join(df, site))"))
  if (is_varkit_linkage_graph(site))
    lifecycle::deprecate_stop("v0.9.0", "mask(site = 'should be boolean')", I("mask(site = is_linked(lgraph, .))"))
  if (!is.logical(site))
    stop("The value of 'site' should be logical/boolean!")

  if (!is.null(why) && !"filter" %in% colnames(x$site)) {
    x$site$filter <- "MASKED"
    x$site$filter[drm_which(drm_get(x$site), "sites")] <- "PASS"
  }

  if (na_mask && anyNA(site))
    site[is.na(site)] <- TRUE

  drm_preselect(drm_get(x$site), sites = !site)

  if (!is.null(why))
    x$site$filter[drm_which(drm_get(x$site), "sites")] <- why

  x
}

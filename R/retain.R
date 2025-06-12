##' @export
retain <- function(x, site, why = NULL, na_retain = TRUE, ...) {
  UseMethod("retain", x)
}

##' @export
retain.varkit <- function(x, site, why = NULL, na_retain = TRUE, ...) {
  if (!is.null(why))
    .stopifnotsingle(why, what = "character")
  .stopifnotsingle(na_retain, what = "logical")

  # dplyr::filter() like argument evaluation (supports: ., site, geno, meta, names(site))
  site <- with_vcf(x, expr = {{ site }})

  if (is.data.frame(site))
    lifecycle::deprecate_stop("v0.9.0", "retain(site = 'should be of type logical')", I("retain(site = has_join(df, site))"))
  if (is_varkit_linkage_graph(site))
    lifecycle::deprecate_stop("v0.9.0", "retain(site = 'should be of type logical')", I("retain(site = is_linked(lgraph, .))"))
  if (!is.logical(site))
    stop("The value of 'site' should be of type logical!")

  if (!is.null(why) && !"filter" %in% colnames(x$site)) {
    x$site$filter <- "MASKED"
    x$site$filter[drm_which(drm_get(x$site), "sites")] <- "PASS"
  }

  if (na_retain && anyNA(site))
    site[is.na(site)] <- TRUE

  drm_preselect(drm_get(x$site), sites = site)

  if (!is.null(why))
    x$site$filter[drm_which(drm_get(x$site), "sites")] <- why

  x
}

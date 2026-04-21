# REVIEW now that we have has_join() & is_linked() all operations can be
# performed using mask() alone, should retain() be deprecated?
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

  if (anyNA(site))
    site[is.na(site)] <- na_retain

  result <- rd_filter(x, sites = .env$site)

  if (!is.null(why)) {
    if (!any("filter" == colnames(rd_unclass(x)$site)))
      result$site$filter <- .if_else(.indices(x)$sites, "PASS", "MASKED")
    result$site$filter[which(.indices(x)$sites)[!site]] <- why
  }

  result
}

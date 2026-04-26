##' @export
mask <- function(x, site, why = NULL, na_mask = TRUE, ...) {
  UseMethod("mask", x)
}

##' @export
mask.varkit <- function(x, site, why = NULL, na_mask = TRUE, ...) {
  if (!is.null(why))
    .stopifnotsingle(why, what = "character")
  .stopifnotsingle(na_mask, what = "logical")

  # dplyr::filter() like argument evaluation (supports: ., site, geno, meta, names(site))
  site <- with_vcf(x, expr = {{ site }})

  if (is.data.frame(site))
    lifecycle::deprecate_stop("v0.9.0", "mask(site = 'should be of type logical')", I("mask(site = has_join(site, df))"))
  if (is_varkit_linkage_graph(site))
    lifecycle::deprecate_stop("v0.9.0", "mask(site = 'should be of type logical')", I("mask(site = is_linked(., linkage_graph))"))
  if (!is.logical(site))
    stop("The value of 'site' should be of type logical!")

  if (anyNA(site))
    site[is.na(site)] <- na_mask

  result <- rd_filter(x, sites = !.env$site)

  if (!is.null(why)) {
    if (!any("filter" == colnames(rd_unclass(x)$site)))
      result$site$filter <- .if_else(.indices(x)$sites, "PASS", "MASKED")
    result$site$filter[which(.indices(x)$sites)[!site]] <- why
  }

  result
}

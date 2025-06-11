##' @export
retain <- function(x, site, why = NULL, na_retain = TRUE, ...) {
  UseMethod("retain", x)
}

##' @export
retain.varkit <- function(x, site, why = NULL, na_retain = TRUE, ...) {
  # dplyr::filter() like argument evaluation (supports: ., site, geno, meta, names(site))
  site <- with_vcf(x, expr = {{ site }})

  if (is.data.frame(site))
    lifecycle::deprecate_stop("v0.9.0", "mask(site = 'should be boolean')", I("mask(site = has_join(df, site))"))
  if (is_varkit_linkage_graph(site))
    lifecycle::deprecate_stop("v0.9.0", "mask(site = 'should be boolean')", I("mask(site = is_linked(lgraph, .))"))
  if (!is.logical(site))
    stop("The value of 'site' should be logical/boolean!")

  mask_site <- !site
  mask.varkit(x, site = mask_site, why = why, na_mask = !na_retain)
}

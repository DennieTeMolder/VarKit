# Eval EXPR with tidy data pronouns (+ ., site, geno, meta, names(site))
##'@export
with_vcf <- function(vcf, expr) {
  stopifnot(is_varkit(vcf))
  expr <- rlang::enquo(expr)

  # Only provide referenced variables to prevent excessive subsetting
  vars <- all.vars(expr)

  # Data for references to 'site' & 'geno'
  vcf_subset <- structure(list(meta = meta.varkit(vcf)), class = "varkit")
  vcf_subset$site <- if ("site" %in% vars) site.varkit(vcf)
  vcf_subset$geno <- if ("geno" %in% vars) geno.varkit(vcf)

  # Data for references to columns of site(vcf)
  site_cols <- intersect(vars, colnames(vcf$site))
  if (length(site_cols) == 0L) {
    site_cols <- NULL
  } else if (!is.null(vcf_subset[["site"]])) {
    site_cols <- as.list(vcf_subset$site)
  } else if (length(site_cols) == 1L) {
    site_cols <- setNames(
      list(site.varkit(vcf, tidyselect::all_of(site_cols))),
      nm = site_cols
    )
  } else {
    site_cols <- as.list(site.varkit(vcf, tidyselect::all_of(site_cols)))
  }

  # The VarKit object in the VCF argument can be referenced with '.'
  rlang::eval_tidy(expr, data = c(list(. = vcf), vcf_subset, site_cols))
}

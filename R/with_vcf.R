# Eval EXPR with tidy data pronouns (+ ., site, geno, meta, names(site))
##'@export
with_vcf <- function(vcf, expr) {
  stopifnot(is_varkit(vcf))
  expr <- rlang::enquo(expr)

  # Only provide referenced variables to prevent excessive subsetting
  vars <- all.vars(expr)

  # Data for references to 'site' & 'geno'
  vcf_subset <- vcf[intersect(c("meta", "site", "geno"), vars)]
  vcf_subset <- linked_data_unclass(vcf_subset)

  # Data for references to columns of site(vcf)
  site_cols <- colnames(linked_data_get(vcf, i = "site", sites = FALSE))
  site_cols <- intersect(vars, site_cols)
  if (length(site_cols) == 0L) {
    site_cols <- NULL
  } else if (any(colnames(vcf_subset) == "site")) {
    site_cols <- as.list(vcf_subset$site)
  } else {
    site_cols <- linked_data_get(vcf, i = "site", , site_cols)
  }

  # The VarKit object in the VCF argument can be referenced with '.'
  rlang::eval_tidy(expr, data = c(list(. = vcf), vcf_subset, site_cols))
}

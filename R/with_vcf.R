# Eval EXPR with tidy data pronouns (+ ., site, geno, meta, names(site))
##'@export
with_vcf <- function(vcf, expr) {
  stopifnot(is_varkit(vcf))
  expr <- rlang::enquo(expr)

  # Only provide referenced variables to prevent excessive subsetting
  vars <- all.vars(expr)

  # Data for references to 'site' & 'geno'
  vcf_subset <- unclass(vcf[intersect(c("meta", "site", "geno"), vars)])

  # Data for references to columns of site(vcf)
  site_cols <- intersect(vars, colnames(linked_data_get(vcf, i = "site", sites = FALSE)))
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

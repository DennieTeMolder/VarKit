# Eval EXPR with tidy data pronouns (+ ., site, geno, meta, names(site))
##'@export
with_vcf <- function(vcf, expr) {
  stopifnot(is_varkit(vcf))
  if (missing(expr))
    stop("The 'expr' argument is empty!")
  expr <- rlang::enquo(expr)

  # The VarKit object in the can be referenced with '.'
  eval_env <- rlang::env(. = vcf)

  # Lazily bind columns of SITE
  bind_vars <- setNames(nm = names(rd_unclass(vcf)$site))
  bind_exprs <- lapply(bind_vars, function(var) {
    rlang::expr(rd_get(vcf, i = "site", , !!var, .drop = TRUE))
  })
  rlang::env_bind_lazy(eval_env, !!!bind_exprs)

  # Lazily bind elements of VCF
  bind_vars <- setNames(nm = names(vcf))
  bind_exprs <- lapply(bind_vars, function(var) {
    rlang::expr(rd_get(vcf, i = !!var))
  })
  rlang::env_bind_lazy(eval_env, !!!bind_exprs)

  # Create data mask and add .data pronoun for tidyverse parity
  eval_env <- rlang::new_data_mask(eval_env)
  eval_env$.data <- rlang::as_data_pronoun(eval_env)

  rlang::eval_tidy(expr, data = eval_env)
}

##'@export
within_vcf <- function(vcf, expr) {
  eval_env <- with_vcf(vcf, {{ expr }}, return_env = TRUE)

  # Retrieve modified variables from EVAL_ENV
  has_new <- ls(eval_env, pattern = "^[^~.]")

  # Variables with no slot become columns of $site
  no_slot <- setdiff(has_new, names(attr(vcf, "slots")))
  if (length(no_slot) > 0L) {
    has_new <- setdiff(has_new, no_slot)

    site <- rd_unclass(vcf)[["site"]]
    site_cols <- colnames(site)

    n_total <- nrow(site)
    n_visible <- n_sites.varkit(vcf)

    # Validate $site if changed before we modify it further
    if (any(has_new == "site")) {
      site <- has_new[["site"]]
      has_new <- has_new[has_new != "site"]
      if (!is.data.frame(site))
        stop("The value of 'site' should be a data frame!")
      if (nrow(site) != n_total) {
        if (nrow(site) == n_visible)
          stop("Unmask before assigning! See `unmask_all()`.")
        stop("Expected 'site' to have ", .frmt_big(n_total),
            "rows, but got ", .frmt_big(nrow(site)), "!")
      }
    }

    # Check if the values make for valid columns
    for (name in no_slot) {
      if (any(name == site_cols))
        next()
      val <- eval_env[[name]]
      if (is.vector(val)) {
        if (any(length(val) == c(1L, n_total)))
          next()
        if (length(val) == n_visible)
          stop("Unmask before assigning! See `unmask_all()`.")
      }
      stop("No slot registered for: '", name, "'!")
    }

    # Assign
    site[no_slot] <- mget(no_slot, envir = eval_env)
    vcf$site <- site
  }

  vcf[has_new] <- mget(has_new, envir = eval_env)
  vcf
}

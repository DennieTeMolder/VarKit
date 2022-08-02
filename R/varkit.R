##' @export
varkit <- function(df, ploidy, header = NULL) {
  # Checks
  stopifnot(is.data.frame(df))
  stopifnot(is.numeric(ploidy))

  # Move fixed fields to a separate df
  fixed_idx <- na.omit(match(.fixed_columns(), colnames(df)))

  if (!ncol(df) > length(fixed_idx))
    stop("Only fixed columns detected in df, missing genotypes!")
  if (!all(c("chrom", "pos") %in% colnames(df)[fixed_idx]))
    stop("Missing 'chrom' and 'pos' columns in df")

  fixed <- df[, fixed_idx]
  df[, fixed_idx] <- NULL

  # The remaining fields are the genotype calls w/ sample name as column.
  # These calls are stored as a transposed df w/ a custom class
  samples <- colnames(df)
  geno <- varkit_geno(df)
  rm(df)

  vcf <- list(
    meta = header$meta,
    fixed = fixed,
    geno = geno,
    ploidy = ploidy,
    samples = samples,
    use_site = rep(TRUE, ncol(geno))
  )
  class(vcf) <- c("varkit", "varkit_header")

  vcf
}

##' @export
print.varkit <- function(x, n = 10) {
  cat("*VarKit Variant Object*\n")
  if (!is.null(x$meta)) {
    print(x$meta, n = n)
    cat("\n")
  }
  cat("Fixed fields (fixed(x)):\n")
  print(x$fixed, n = n)
  cat("\nTransposed genotype matrix (geno(x)):\n")
  print(x$geno, n = n)
  cat("\nWith", length(x$samples), "samples (samples(x)):\n")
  print(x$samples)
  cat("\nCurrently selected", sum(x$use_site),
      "out of", length(x$use_site), "sites.\n")
}

##' @export
summary.varkit <- function(x) {
  tibble::tibble(
    samples = length(x$samples),
    sites_visible = sum(x$use_site),
    sites_total = length(x$use_site),
    sites_frac = mean(x$use_site)
  )
}

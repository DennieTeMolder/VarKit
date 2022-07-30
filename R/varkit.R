##' @export
varkit <- function(df, header = NULL) {
  # Checks
  if (!is.data.frame(df))
    stopifnot(is.data.frame(df))

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
    header = header[names(header) != "col_names"],
    fixed = fixed,
    geno = geno,
    samples = samples,
    use_site = rep(TRUE, ncol(geno)),
    use_sample = rep(TRUE, length(samples))
  )
  class(vcf) <- "varkit"

  vcf
}

##' @export
print.varkit <- function(x) {
  cat("*VarKit Variant Object*\n")
  if (!is.null(x$header)) {
    cat("Header:\n")
    print(x$header)
    cat("\n")
  }
  cat("Fixed fields:\n")
  print(x$fixed)
  cat("Transposed genotype matrix:\n")
  print(x$geno)
  cat("\nSamples:\n")
  print(x$samples)
  cat("\n")
  cat("Currently selected", sum(x$use_sample),
      "out of", length(x$use_sample), "samples.\n")
  cat("Currently selected", sum(x$use_site),
      "out of", length(x$use_site), "sites.\n")
}

##' @export
summary.varkit <- function(x) {
  tibble::tibble(
    rowname = c("sites", "samples"),
    used = c(sum(x$use_site), sum(x$use_sample)),
    total = c(length(x$use_site), length(x$use_sample)),
    fraction = c(mean(x$use_site), mean(x$use_sample))
  )
}

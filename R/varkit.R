##' @export
varkit <- function(df, header = NULL) {
  # Checks
  stopifnot(is.data.frame(df))
  if (!is.null(header))
    stopifnot(is.list(header))

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
    use_site = rep(TRUE, ncol(geno))
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
  cat("\nSamples (", length(x$samples), " total):\n", sep = "")
  print(x$samples)
  cat("\n")
  cat("Currently selected", sum(x$use_site),
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

##' @export
varkit <- function(df, header = NULL, spec = NULL) {
  if (!is.data.frame(df))
    stopifnot(is.data.frame(df))
  if (!is.null(header)) {
    if (!(tibble::is_tibble(header) && all(colnames(header) == c("name", "value"))))
      stop("Header should be a tibble with columns 'name' and 'value'")
  }
  if (!is.null(spec))
    stopifnot(is.character(spec))

  fixed_idx <- na.omit(match(.fixed_columns(), colnames(df)))

  if (!ncol(df) > length(fixed_idx))
    stop("Only fixed columns detected in df, missing genotypes!")
  if (!all(c("chrom", "pos") %in% colnames(df)[fixed_idx]))
    stop("Missing 'chrom' and 'pos' columns in df")

  fixed <- df[, fixed_idx]
  df[, fixed_idx] <- NULL
  geno <- varkit_geno(df)
  rm(df)

  vcf <- list(
    spec = spec,
    header = header,
    fixed = fixed,
    geno = geno,
    use_sites = 1:ncol(geno),
    use_samples = 1:nrow(geno)
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
  cat("\n")
  print(x$geno)
  cat("\nSamples:\n")
  print(samples(x$geno))
  cat("\nCurrently selected", length(x$use_samples), "out of", nrow(x$geno), "samples.\n")
  cat("Currently selected", length(x$use_sites), "out of", ncol(x$geno), "sites.\n")
}

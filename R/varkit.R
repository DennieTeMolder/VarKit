##' @export
varkit <- function(vcf, meta = NULL) {
  # Checks
  stopifnot(is.data.frame(vcf))
  if (!is.null(meta))
    stopifnot(is.data.frame(meta), colnames(meta) == c("name", "value"))

  # Detect fixed fields
  fixed_idx <- stats::na.omit(match(.fixed_columns(), colnames(vcf)))

  if (length(fixed_idx) == 0)
    stop("No fixed columns detected in VCF, ensure fixed column names are lower case!")
  if (ncol(vcf) <= length(fixed_idx))
    stop("Only fixed columns detected in VCF, missing genotypes!")
  if (!all(c("chrom", "pos") %in% colnames(vcf)[fixed_idx]))
    stop("Missing 'chrom' and/or 'pos' columns in VCF!")

  # Move fixed fields to a separate df
  fxd <- vcf[, fixed_idx]
  vcf[, fixed_idx] <- NULL

  # The remaining fields are the genotype calls, which are stored as a
  # transposed matrix w/ a custom class
  geno <- varkit_geno(vcf)
  rm(vcf)

  # Apply initial mask based on the FILTER column
  use_site <- FALSE
  if ("filter" %in% names(fxd))
    use_site <- fxd$filter == "PASS" | is.na(fxd$filter)
  if (!any(use_site))
    use_site <- rep(TRUE, ncol(geno))

  vcf <- list(
    meta = meta,
    fixed = fxd,
    geno = geno,
    use_site = use_site
  )
  class(vcf) <- c("varkit", "varkit_header", "list")

  vcf
}

##' @export
is_varkit <- function(x) {
  inherits(x, "varkit")
}

##' @export
print.varkit <- function(x, ..., n = .max_print_col(geno(x))) {
  cat("*VarKit Variant Object*\n")
  if (!is.null(x$meta)) {
    cat("Header metadata, `meta(vcf)`:\n")
    print(meta(x), ..., verbose = FALSE)
    cat("\n")
  }
  cat("Fixed fields, `fixed(vcf)`:\n")
  print(fixed(x), ..., n = n)
  cat("\nTransposed genotype matrix, `geno(vcf)`:\n")
  print(geno(x), ..., n = n)
  cat("# Use `samples(vcf)` for a vector of all sample names\n")
  cat("\nCurrently selected", sum(x$use_site), "out of",
      length(x$use_site), "sites, see `summary(vcf)`\n")
}

##' @export
summary.varkit <- function(object, ...) {
  tibble::tibble(
    samples = length(samples(object)),
    sites_visible = sum(object$use_site),
    sites_total = length(object$use_site),
    visible_frac = mean(object$use_site)
  )
}

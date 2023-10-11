##' @export
varkit <- function(vcf, meta = NULL) {
  # Checks
  stopifnot(is.data.frame(vcf), nrow(vcf) > 0)
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
    site = fxd,
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
print.varkit <- function(x, ..., n = NULL, n_samples = 10) {
  if (is.null(n)) {
    width <- getOption("width")
    which_show <- utils::head(which(x$use_site), n = max(1, floor(width / 2) - 1))
    n <- .max_print_col(x$geno[seq_len(n_samples), which_show], width = width)
  }

  .msg_h1("VarKit Variant Object")
  if (!is.null(x$meta)) {
    .msg_h2("Header metadata, `meta(vcf)`")
    print(meta(x), ..., verbose = FALSE)
    cat("\n")
  }

  .msg_h2("Sample names, `samples(vcf)`")
  samp <- cli::cli_vec(samples(x), style = list("vec-sep" = ", ", "vec-last" = ", ", "vec-trunc" = 100))
  cli::cli_text("{samp}")
  cat("\n")

  .msg_h2("Site information, `site(vcf)`")
  print(site(x), ..., n = n)
  cat("\n")

  .msg_h2("Transposed genotype matrix, `geno(vcf)`")
  print(geno(x), ..., n = n, n_samples = n_samples)
  cat("\n")

  cli::cli_alert_warning(c("Currently selected {sum(x$use_site)}, out of ",
                           "{length(x$use_site)} sites, see `summary(vcf)`"))
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

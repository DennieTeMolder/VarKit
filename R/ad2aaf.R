##' @export
ad2aaf <- function(vcf, type = "AD") {
  stopifnot(is_varkit(vcf))

  if (is.null(type)) {
    ad <- geno.varkit(vcf)
  } else {
    # Extract allelic depth (AD)
    ad <- parse_geno.varkit(vcf, type = type, allow_missing = FALSE)
  }

  if (!is.character(ad))
    stop(if (is.null(type)) "geno(vcf)" else "parse_geno(vcf, type)", " should return a character matrix!")

  d_names <- dimnames(ad)
  n_site <- ncol(ad)
  n_sample <- nrow(ad)
  n_delim <- stringr::str_count(ad[1L, ], stringr::fixed(","))
  n_allele <- max(n_delim) + 1L

  if (n_allele < 2L)
    stop("Could not detect multiple allelic depths delimited by a ','!")

  # Split into a vector where the columns are the read depth per allele (can be NA)
  ad <- stringr::str_split_fixed(ad, stringr::fixed(","), n = n_allele)
  storage.mode(ad) <- "integer"

  # Index of the alternative allele to use
  alt_idx <- rep(1L, n_site)

  # For multi-allelic sites, use the most abundant alt allele
  is_multi <- n_delim > 1L
  if (any(is_multi)) {
    start_multi <- (which(is_multi) - 1L) * n_sample + 1L
    multi_alt_sums <- t(mapply(function(start, end) {
      matrixStats::colSums2(ad, cols = -1L, rows = seq(start, end), useNames = FALSE)
    }, start_multi, start_multi + (n_sample - 1L)))

    multi_alt_rank <- matrixStats::rowRanks(multi_alt_sums, ties.method = "last", useNames = FALSE)
    multi_alt_idx <- which(multi_alt_rank == n_delim[is_multi], arr.ind = TRUE, useNames = FALSE)
    multi_alt_idx <- multi_alt_idx[, 2L][order(multi_alt_idx[, 1L])]

    alt_idx[is_multi] <- multi_alt_idx
    rm(start_multi, multi_alt_sums, multi_alt_rank, multi_alt_idx)
  }

  # Read depth of the most abundant alternative allele
  alt_dp <- matrixStats::rowCollapse(ad, rep(alt_idx + 1L, each = n_sample), useNames = FALSE)
  # Bi-allelic read depth
  bi_dp <- ad[, 1L] + alt_dp
  # Fraction of total reads that support bi-allelic sites
  # 0 if there are only multi-allelic reads, NaN if there are no reads
  bi_frac <- bi_dp / rowSums(ad, na.rm = TRUE)
  # Alternative Allele Fraction (AAF)
  aaf <- alt_dp / bi_dp

  list(
    aaf = varkit_geno(matrix(aaf, nrow = n_sample, dimnames = d_names)),
    bi_dp = varkit_geno(matrix(bi_dp, nrow = n_sample, dimnames = d_names)),
    bi_frac = varkit_geno(matrix(bi_frac, nrow = n_sample, dimnames = d_names)),
    alt_idx = alt_idx
  )
}

##' @export
ad2aaf <- function(vcf, type = "AD") {
  stopifnot(is_varkit(vcf))

  if (is.null(type)) {
    ad <- geno(vcf)
  } else {
    # Extract allelic depth (AD)
    ad <- parse_geno(vcf, type)
  }

  if (storage.mode(ad) != "character")
    stop(if (is.null(type)) "geno(vcf)" else "parse_geno(vcf, type)", " should return a character matrix!")

  n_sites <- ncol(ad)
  n_samples <- nrow(ad)
  n_allele <- max(stringr::str_count(ad[1L, ], stringr::fixed(","))) + 1L

  if (n_allele < 2L)
    stop("Could not detect multiple allelic depths delimited by a ','!")

  # Variables for storing results
  aaf <- matrix(NA_real_, nrow = n_samples, ncol = n_sites, dimnames = dimnames(ad))
  bi_dp <- matrix(NA_integer_, nrow = n_samples, ncol = n_sites, dimnames = dimnames(ad))
  bi_frac <- matrix(NA_real_, nrow = n_samples, ncol = n_sites, dimnames = dimnames(ad))
  alt_idx <- vector("integer", n_sites)

  # Split into one matrix per site of [samples x alleles]
  ad <- stringr::str_split_fixed(ad, stringr::fixed(","), n = n_allele)
  storage.mode(ad) <- "integer"
  ad <- split.data.frame(ad, rep(seq_len(n_sites), each = n_samples))
  names(ad) <- NULL

  for (i in seq_along(ad)) {
    current <- ad[[i]]
    is_na <- is.na(current[1L, ])
    if (any(is_na))
      current <- current[, !is_na, drop = FALSE]

    if (ncol(current) == 2L) {
      # Simple case if current is bi-allelic (bi_dp == dp)
      current_idx <- 2L
      current_dp <- current[, 1L] + current[, current_idx]
      current_frac <- ifelse(current_dp > 0L, 1, NaN)
    } else {
      # For multi-allelic sites, use the most abundant alt allele
      current_idx <- which.max(colSums(current[, -1L, drop = FALSE])) + 1L
      # Bi-allelic read depth
      current_dp <- current[, 1L] + current[, current_idx]
      # Fraction of total reads that support bi-allelic sites
      # 0 if there are only multi-allelic reads, NA if there are no reads
      current_frac <- current_dp / rowSums(current)
    }

    # Alternative Allele Fraction (AAF)
    aaf[, i] <- current[, current_idx] / current_dp
    bi_dp[, i] <- current_dp
    bi_frac[, i] <- current_frac
    alt_idx[i] <- current_idx - 1L
  }

  list(aaf = varkit_geno(aaf), bi_dp = varkit_geno(bi_dp), bi_frac = varkit_geno(bi_frac), alt_idx = alt_idx)
}

##' @export
ad2aaf <- function(vcf, ad_cap = NULL, type = "AD", alt_idx = NULL) {
  stopifnot(is_varkit(vcf))
  if (!is.null(ad_cap))
    stopifnot(is.numeric(ad_cap), length(ad_cap) == 1, ad_cap >= 1)
  if (!is.null(alt_idx))
    stopifnot(is.numeric(alt_idx), is.vector(alt_idx), length(alt_idx) == n_sites(vcf))

  # Retrieve allelic depth (AD)
  if (is.null(type)) {
    ad <- geno.varkit(vcf)
  } else {
    ad <- parse_geno.varkit(vcf, type = type, allow_missing = FALSE)
  }

  if (!is.matrix(ad) || !is.character(ad))
    stop(if (is.null(type)) "geno(vcf)" else "parse_geno(vcf, type)", " should return a character matrix!")

  d_names <- dimnames(ad)
  n_site <- ncol(ad)
  n_sample <- nrow(ad)
  n_delim <- stringr::str_count(ad[1L, ], stringr::fixed(","))
  n_allele <- max(n_delim) + 1L

  if (isTRUE(n_allele < 2L))
    stop("Could not detect multiple allelic depths delimited by a ','!")

  # Split into a 2d matrix with the shape [n_sample * n_site, n_allele]
  ad <- stringr::str_split_fixed(ad, stringr::fixed(","), n = n_allele)
  storage.mode(ad) <- "integer"

  # Check if there are site with multiple ALT alleles
  is_multi <- n_delim > 1L
  if (any(is_multi)) {
    if (is.null(alt_idx)) {
      # For multi-allelic sites pick the alt allele w/ the highest depth
      alt_idx <- rep(1L, n_site)

      # Get all multi-allelic sites by converting to a 3d matrix
      # The shape is the same to the geno() matrix, with the z-axis holding the alleles
      dim(ad) <- c(n_sample, n_site, n_allele)
      multi_ad <- ad[, is_multi, -1L, drop = FALSE]
      dim(ad) <- c(n_sample * n_site, n_allele)
      multi_ad[is.na(multi_ad)] <- 0L

      # Cap and scale down samples with a high depth for single alleles
      if (!is.null(ad_cap)) {
        # Convert to a 2d matrix
        dim(multi_ad) <- with(list(x = dim(multi_ad)), c(x[1L] * x[2L], x[3L]))
        maxs <- matrixStats::rowMaxs(multi_ad)

        # Adjust all alleles per sample such that the ratio stays the same
        above_cap <- maxs > ad_cap
        if (any(above_cap)) {
          adjusted <- ad_cap / maxs[above_cap] * multi_ad[above_cap, ]
          storage.mode(adjusted) <- "integer"
          multi_ad[above_cap, ] <- adjusted
          rm(adjusted)
        }

        # Convert back to 3d for the colSums()
        dim(multi_ad) <- c(n_sample, sum(is_multi), n_allele - 1L)
        rm(maxs, above_cap)
      }

      # Sums the depth over all samples per site per allele
      # We use an array to avoid having the apply colSums over each site
      multi_ad <- colSums(multi_ad)

      # Find the index of the allele with the highest total depth
      multi_idx <- matrixStats::rowRanks(multi_ad, ties.method = "last")
      multi_idx <- which(multi_idx == ncol(multi_idx), arr.ind = TRUE, useNames = FALSE)
      alt_idx[is_multi] <- multi_idx[, 2L][order(multi_idx[, 1L])]
      rm(multi_ad, multi_idx)
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
  } else {
    # Simple case
    alt_idx <- rep(1L, n_site)
    bi_dp <- ad[, 1L] + ad[, 2L]
    aaf <- ad[, 2L] / bi_dp
    bi_frac <- rep(1L, length(aaf))
  }

  list(
    aaf = varkit_geno(matrix(aaf, nrow = n_sample, dimnames = d_names)),
    bi_dp = varkit_geno(matrix(bi_dp, nrow = n_sample, dimnames = d_names)),
    bi_frac = varkit_geno(matrix(bi_frac, nrow = n_sample, dimnames = d_names)),
    alt_idx = alt_idx
  )
}

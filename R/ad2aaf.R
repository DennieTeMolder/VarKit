##' @export
ad2aaf <- function(vcf, ad_cap = NULL, type = "AD", alt_idx = NULL) {
  stopifnot(is_varkit(vcf))
  if (!is.null(ad_cap)) {
    .stopifnotsingle(ad_cap, what = is.numeric)
    stopifnot(ad_cap >= 1)
  }
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
  n_delim <- stringr::str_count(get_first_non_na(ad, margin = 2), stringr::fixed(","))
  n_allele <- max(n_delim, 0L, na.rm = TRUE) + 1L

  if (isTRUE(n_allele < 2L))
    stop("Could not detect multiple allelic depths delimited by a ','!")

  # Split into a 2d matrix with the shape [n_sample * n_site, n_allele]
  ad <- stringr::str_split_fixed(ad, stringr::fixed(","), n = n_allele)
  storage.mode(ad) <- "integer"

  # NAs can represented as zero depth, so we don't have to account for them separately
  if (anyNA(ad))
    ad[is.na(ad)] <- 0L

  # Check if there are sites with multiple ALT alleles
  which_multi <- which(n_delim > 1L)
  if (length(which_multi) != 0L) {
    if (is.null(alt_idx)) {
      # For multi-allelic sites pick the alt allele w/ the highest depth
      alt_idx <- rep(1L, n_site)

      # Get all multi-allelic sites by converting to a 3d matrix
      # The shape is the same to the geno() matrix, with the z-axis holding the alleles
      dim(ad) <- c(n_sample, n_site, n_allele)
      multi_ad <- ad[, which_multi, -1L, drop = FALSE]
      dim(ad) <- c(n_sample * n_site, n_allele)

      # Cap and scale down samples with a high depth for single alleles
      if (!is.null(ad_cap)) {
        # Convert to a 2d matrix to find max AD per call
        dim_3d <- dim(multi_ad)
        dim(multi_ad) <- c(dim_3d[1L] * dim_3d[2L], dim_3d[3L])
        maxs <- matrixStats::rowMaxs(multi_ad)

        # Adjust all alleles per sample such that the ratio stays the same
        which_above <- which(maxs > ad_cap)
        if (length(which_above) != 0L) {
          multi_ad[which_above, ] <- as.integer(
            multi_ad[which_above, ] * ad_cap / maxs[which_above]
          )
        }

        # Convert back to 3d for the colSums()
        dim(multi_ad) <- dim_3d
        rm(maxs, which_above)
      }

      # Sums the depth over all samples per site per allele
      # We use an array to avoid having the apply colSums over each site
      multi_ad <- colSums(multi_ad)

      # Find the index of the allele with the highest total depth
      alt_idx[which_multi] <- which_nth_lowest(multi_ad * -1L, margin = 1, n = 1L)
      rm(multi_ad)
    }

    # Read depth of the most abundant alternative allele
    alt_dp <- ad[seq_len(nrow(ad)) + rep(alt_idx * nrow(ad), each = n_sample)]
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
    bi_frac <- rep(NaN, length(aaf))
    bi_frac[bi_dp > 0L] <- 1
  }

  list(
    aaf = varkit_geno(matrix(aaf, nrow = n_sample, dimnames = d_names)),
    bi_dp = varkit_geno(matrix(bi_dp, nrow = n_sample, dimnames = d_names)),
    bi_frac = varkit_geno(matrix(bi_frac, nrow = n_sample, dimnames = d_names)),
    alt_idx = alt_idx
  )
}

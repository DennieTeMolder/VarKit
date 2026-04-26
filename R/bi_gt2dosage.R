##' @export
bi_gt2dosage <- function(vcf, parse_geno_type = "FORMAT/GT") {
  stopifnot(is_varkit(vcf))

  if (is.null(parse_geno_type)) {
    mat <- geno.varkit(vcf)
  } else {
    mat <- parse_geno.varkit(vcf, type = parse_geno_type, allow_missing = FALSE)
  }

  if (!is.matrix(mat) || !is.character(mat))
    stop("Expected parse_geno(vcf, parse_geno_type) to return a character matrix!")

  # Unclass so primitive functions are used everywhere
  dims <- dim(mat)
  dnames <- dimnames(mat)
  mat <- unclass(mat)

  n_delim <- stringr::str_count(get_first_non_na(mat, margin = 2), pattern = "[/|]")
  n_allele <- max(n_delim, 0L, na.rm = TRUE) + 1L

  # Sum up the 1's to get dosage (NA if any additional alleles are present)
  if (n_allele == 1L) {
    # Simple case without delimiters
    mat <- as.integer(mat)
    not_bi_allelic <- mat > 1L
    mat[not_bi_allelic] <- NA
  } else {
    # Complex case with delimiters
    mat <- stringr::str_split_fixed(mat, pattern = "[/|]", n = n_allele)
    storage.mode(mat) <- "integer"
    not_bi_allelic <- matrixStats::rowAnys(mat > 1L, na.rm = TRUE)
    is_na <- is.na(mat[, 1L, drop = TRUE])
    mat <- rowSums(mat, na.rm = TRUE)
    mat[not_bi_allelic | is_na] <- NA
  }

  # Restore attributes
  dim(mat) <- dims
  as_mtrx(mat, dimnames = dnames)
}

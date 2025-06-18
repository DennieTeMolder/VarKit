# Ploidy is determined by length(cutoffs)
# Example: num2gt(vcf$geno, c(0.25, 0.75))
##' @export
num2gt <- function(x, cutoffs, sep = "/") {
  stopifnot(is.numeric(x), is.vector(x) || is.matrix(x))
  stopifnot(is.numeric(cutoffs))
  .stopifnotsingle(sep, what = "character")

  res <- rep("0", length(x))
  res <- lapply(sort(cutoffs, decreasing = TRUE), function(cutoff) {
    gt <- res
    gt[x >= cutoff] <- "1"
    gt
  })

  res <- do.call(paste, c(res, list(sep = sep)))
  if (.any_na(x))
    res[is.na(x)] <- NA_character_

  .restore_array(res, using = x)
}

# Ploidy is determined by length(cutoffs)
# Example: num2gt(vcf$geno, c(0.25, 0.75))
##' @export
num2gt <- function(x, cutoffs, sep = "/") {
  stopifnot(is.numeric(x), is.numeric(cutoffs))
  .stopifnotsingle(sep, what = "character")

  res <- rep("0", length(x))
  res <- lapply(sort(cutoffs, decreasing = TRUE), function(cutoff) {
    gt <- res
    gt[x >= cutoff] <- "1"
    gt
  })

  .join <- function(...) paste(..., sep = sep)
  res <- do.call(.join, res)

  res[is.na(x)] <- NA_character_

  attributes(res) <- attributes(x)
  res
}

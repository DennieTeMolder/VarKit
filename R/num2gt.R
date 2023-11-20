# Ploidy is determined by length(cutoffs)
# Example: num2gt(vcf$geno, c(0.25, 0.75))
##' @export
num2gt <- function(x, cutoffs, sep = "/") {
  stopifnot(is.numeric(x), is.numeric(cutoffs))
  stopifnot(is.character(sep), length(sep) == 1)

  cutoffs <- sort(cutoffs, decreasing = TRUE)
  res <- lapply(cutoffs, function(cutoff) {
    ifelse(is.na(x), ".", ifelse(x >= cutoff, "1", "0"))
  })

  .join <- function(...) paste(..., sep = sep)
  res <- do.call(.join, res)

  attributes(res) <- attributes(x)
  res
}

##' @export
varkit_geno <- function(x) {
  UseMethod("varkit_geno", x)
}

##' @export
varkit_geno.data.frame <- function(x) {
  x <- t(x)
  class(x) <- c("varkit_geno", "matrix", "array")
  x
}

##' @export
varkit_geno.matrix <- function(x) {
  class(x) <- c("varkit_geno", "matrix", "array")
  x
}

##' @export
print.varkit_geno <- function(x, n_sites = 5, n_samples = 5) {
  cat("# A", class(x[[1,1]]), "matrix\n")
  print(x[1:n_samples, 1:n_sites])
  cat("# with a total of", ncol(x), "sites x", nrow(x), "samples and",
       sum(is.na(x)), "missing values\n")
}

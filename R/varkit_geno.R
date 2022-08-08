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
print.varkit_geno <- function(x, ..., n_sites = 5, n_samples = 5) {
  n_col <- ncol(x)
  n_row <- nrow(x)
  n_sites <- min(c(n_sites, n_col))
  n_samples <- min(c(n_samples, n_row))
  cat("# A", class(x[[1,1]]), "matrix\n")
  print(x[1:n_samples, 1:n_sites], ...)
  cat("# with a total of", n_col, "sites x", n_row, "samples and",
       sum(is.na(x)), "missing values\n")
}

##' @export
as.data.frame.varkit_geno <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame.matrix(t(x), row.names = row.names, optional = optional, ...)
}

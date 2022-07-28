##' @export
varkit_geno <- function(x) {
  UseMethod("varkit_geno", x)
}

##' @export
varkit_geno.matrix <- function(x) {
  class(x) <- c("varkit_geno", "matrix", "array")
  x
}

##' @export
varkit_geno.data.frame <- function(x) {
  x <- t(as.matrix(x))
  varkit_geno(x)
}

##' @export
print.varkit_geno <- function(geno, n = 10) {
  cat("Genotype matrix with", ncol(geno), "sites and", nrow(geno), "samples:\n")
  table <- to_tibble(get_idx(geno, site = 1:n))
  print(table, n = n)
}

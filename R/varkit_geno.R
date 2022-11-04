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
t.varkit_geno <- function(x) {
  class(x) <- c("matrix", "array")
  t(x)
}

##' @export
is_varkit_geno <- function(x) {
  inherits(x, "varkit_geno")
}

##' @export
print.varkit_geno <- function(x, ..., n = .max_print_col(x)) {
  if (n > ncol(x))
    n <- ncol(x)

  cat("# A", class(x[1]), "matrix:",
      ncol(x), "sites x", nrow(x), "samples;", sum(is.na(x)), "NAs\n")

  print(x[, seq_len(n), drop = FALSE], ...)

  if (n < ncol(x))
    cat("# Use `print(n = ...)` to see more sites\n")

  invisible(x)
}

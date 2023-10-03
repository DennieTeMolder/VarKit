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
print.varkit_geno <- function(x, ..., n = .max_print_col(x), n_samples = 10) {
  if (n > ncol(x))
    n <- ncol(x)
  if (n_samples > nrow(x))
    n_samples <- nrow(x)


  cat("# A", class(x[1]), "matrix:",
      nrow(x), "samples x", ncol(x), "sites;", sum(is.na(x)), "NAs\n")

  print(x[seq_len(n_samples), seq_len(n), drop = FALSE], ...)

  if (n_samples < nrow(x)) {
    .msg_info(nrow(x) - n_samples, " more samples")
    .msg_info("Use `print(n_samples = ...)` to see more samples")
  }
  if (n < ncol(x))
    .msg_info("Use `print(n = ...)` to see more sites")

  invisible(x)
}


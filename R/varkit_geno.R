##' @export
varkit_geno <- function(x) {
  UseMethod("varkit_geno", x)
}

##' @export
varkit_geno.data.frame <- function(x) {
  varkit_geno.matrix(t(as.matrix(x)))
}

##' @export
varkit_geno.matrix <- function(x) {
  class(x) <- unique(c("varkit_geno", class(x)))
  x
}

##' @export
is_varkit_geno <- function(x) {
  inherits(x, "varkit_geno")
}

##' @export
print.varkit_geno <- function(x, ..., n = .max_print_col(x), n_samples = 25) {
  site_trunc <- n < ncol(x)
  sample_trunc <- n_samples < nrow(x)
  if (!site_trunc)
    n <- ncol(x)
  if (!sample_trunc)
    n_samples <- nrow(x)

  cat("# A", typeof(x), "matrix:", .frmt_big(nrow(x)), "samples x",
      .frmt_big(ncol(x)), "sites;", .frmt_big(sum(is.na(x))), "NAs\n")

  print(unclass(x[seq_len(n_samples), seq_len(n), drop = FALSE]), ...)

  if (sample_trunc)
    .msg_info(nrow(x) - n_samples, " more samples")

  if (site_trunc && sample_trunc) {
    .msg_info("Use `print(n = ..., n_samples = ...)` to see more sites and samples respectively.")
  } else if (site_trunc) {
    .msg_info("Use `print(n = ...)` to see more sites")
  } else if (sample_trunc) {
    .msg_info("Use `print(n_samples = ...)` to see more samples")
  }

  invisible(x)
}

##' @export
`[.varkit_geno` <- function(x, ...) {
  res <- NextMethod()
  if (is.matrix(res))
    class(res) <- class(x)
  res
}

##' @export
t.varkit_geno <- function(x) {
  t(unclass(x))
}

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
  structure(x, class = unique(c("varkit_geno", class(x))))
}

##' @export
is_varkit_geno <- function(x) {
  inherits(x, "varkit_geno")
}

##' @export
print.varkit_geno <- function(x, ..., n = .max_print_col(x), n_samples = 25) {
  site_trunc <- n < n_sites.varkit_geno(x)
  sample_trunc <- n_samples < n_samples.varkit_geno(x)
  if (!site_trunc)
    n <- n_sites.varkit_geno(x)
  if (!sample_trunc)
    n_samples <- n_samples.varkit_geno(x)

  cat(sprintf(
    "# A matrix<%s>: %s samples x %s sites; %.2f%% is NA\n",
    typeof(x), .frmt_big(nrow(x)), .frmt_big(ncol(x)), mean(is.na(x)) * 100
  ))

  print(unclass(x[seq_len(n_samples), seq_len(n), drop = FALSE]), ...)

  if (sample_trunc)
    .msg_info(n_samples.varkit_geno(x) - n_samples, " more samples")

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

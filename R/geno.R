##' @export
geno <- function(x) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x) {
  varkit_filter(x$geno, site = x$use_site)
}

##' @export
`geno<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`geno() <-` is only supported for 'varkit' objects!")
  if (is.vector(value) && length(value) == length(x$geno)) {
    warning("Converting vector to matrix, check if the values are in the right row & column.")
    value <- matrix(value, nrow = nrow(x$geno), ncol = ncol(x$geno), dimnames = dimnames(x$geno))
  } else if (!is.matrix(value)) {
    stop("The new value should be a matrix!")
  }
  if (ncol(x$geno) != ncol(value)) {
    if (!all(x$use_site))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The new value should have an equal number of columns!\n",
         "Use `mask()/retain()` (+ `remove_masked()`) to remove columns/sites.")
  }

  x$geno <- varkit_geno(value)
  validate_varkit(x)
}

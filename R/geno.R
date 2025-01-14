##' @export
geno <- function(x, ..., .drop = FALSE) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x, ..., .drop = FALSE) {
  select_sites <- !all(x$use_site)
  select_samples <- ...length() > 0L

  if (select_samples) {
    .data <- samples.varkit(x)
    .data <- setNames(seq_along(.data), nm = .data)
    loc <- tidyselect::eval_select(rlang::expr(c(...)), data = .data)
  }

  # The `extract` function is slower when dimensions are NULL opposed to missing()
  if (select_samples && select_sites) {
    out <- x$geno[loc, x$use_site, drop = .drop]
  } else if (select_samples) {
    out <- x$geno[loc, , drop = .drop]
  } else if (select_sites) {
    out <- x$geno[, x$use_site, drop = .drop]
  } else {
    out <- x$geno
  }

  # Enable tidyselect renaming of rows
  if (select_samples && is_varkit_geno(out)) {
    rownames(out) <- names(loc)
  }

  out
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

##' @export
geno <- function(x, ..., .drop = FALSE) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x, ..., .drop = FALSE) {
  if (...length() > 0L) {
    .data <- samples.varkit(x)
    .data <- setNames(seq_along(.data), nm = .data)
    sample_select <- tidyselect::eval_select(rlang::expr(c(...)), data = .data)

    res <- drm_do_subset(x$geno, sample_select, , .drop = .drop)
    if (!is.vector(res))
      rownames(res) <- names(sample_select)
  } else {
    res <- drm_do_subset(x$geno, .drop = .drop)
  }

  res
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
    if (drm_n_total(x) != drm_n_selected(x))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The new value should have an equal number of columns!\n",
         "Use `mask()/retain()` (+ `remove_masked()`) to remove columns/sites.")
  }

  varkit(
    meta = x$meta,
    site = x$site,
    geno = varkit_geno(value),
    drmodel = drm_get(x)
  )
}

##' @export
geno <- function(x, ..., .drop = FALSE) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x, ..., .drop = FALSE) {
  if (...length() > 0L) {
    .data <- setNames(which(.indices(x)$samples), nm = samples.varkit(x))
    loc <- tidyselect::eval_select(rlang::expr(c(...)), data = .data)

    res <- linked_data_get(x, i = "geno", samples = loc, , .drop = .drop)
    if (!is.vector(res))
      rownames(res) <- names(loc)
  } else {
    res <- linked_data_get(x, i = "geno", .drop = .drop)
  }

  res
}

##' @export
`geno<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`geno() <-` is only supported for 'varkit' objects!")

  xgeno <- linked_data_unclass(x)$geno

  if (is.vector(value) && length(value) == length(xgeno)) {
    warning("Converting vector to matrix, check if the values are in the right row & column.")
    value <- mtrx(value, nrow = nrow(xgeno), ncol = ncol(xgeno), dimnames = dimnames(xgeno))
  } else if (!is.matrix(value)) {
    stop("The new value should be a matrix!")
  }

  if (ncol(xgeno) != ncol(value)) {
    if (!all(.indices(x)$sites))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The new value should have an equal number of columns/sites!\n",
         "Use `mask()/retain()` (+ `delete_masked()`) to remove sites.")
  }

  # Remove the old value, and attempt to free the 'samples' index so it can be modified
  x$geno <- NULL
  x <- linked_data_trim(x, indices = "samples")

  x$geno <- varkit_geno(value)
  validate_varkit(x)
}

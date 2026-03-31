##' @export
samples <- function(x) {
  UseMethod("samples", x)
}

##' @export
samples.varkit <- function(x) {
  rd_dimnames(x, i = "geno")[["samples"]]
}

##' @export
samples.varkit_header <- function(x) {
  x$col_names[!tolower(x$col_names) %in% .fixed_columns()]
}

##' @export
`samples<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`samples()<-` only works on 'varkit' objects!")
  validate_varkit(x)

  curr_ind <- .indices(x)$samples
  if (length(value) != length(curr_ind)) {
    if (!all(curr_ind))
      stop("Reset/unmask the 'samples' filter before modifying!")
    stop("The new value should be of equal length to the old!")
  }
  if (anyDuplicated(value) != 0L)
    stop("The new value contains duplicates!")

  restore_class <- class(x)
  x <- unclass(x)

  rownames(x$geno) <- value

  class(x) <- restore_class
  x
}

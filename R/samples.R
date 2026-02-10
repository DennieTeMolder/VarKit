##' @export
samples <- function(x) {
  UseMethod("samples", x)
}

##' @export
samples.varkit <- function(x) {
  linked_data_dimnames(x, i = "geno")[["samples"]]
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

  if (!all(.indices(x)$samples))
    stop("Reset/unmask the 'samples' filter before modifying!")
  if (length(value) != length(.indices(x)$samples))
    stop("The new value should be of equal length to the old!")
  if (anyDuplicated(value) != 0L)
    stop("The new value contains duplicates!")

  rownames(x$geno) <- value
  x
}

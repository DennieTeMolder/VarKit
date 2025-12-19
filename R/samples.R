##' @export
samples <- function(x) {
  UseMethod("samples", x)
}

##' @export
samples.varkit_geno <- function(x) {
  rownames(x)
}

##' @export
samples.varkit <- function(x) {
  rownames(linked_data_get(x, i = "geno", sites = FALSE))
}

##' @export
samples.varkit_header <- function(x) {
  x$col_names[!tolower(x$col_names) %in% .fixed_columns()]
}

##' @export
`samples<-` <- function(x, value) {
  if (anyDuplicated(value) != 0L)
    stop("The new value contains duplicates!")

  if (is_varkit(x)) {
    validate_varkit(x)
    if (!all(.indices(x)$samples))
      stop("Reset/unmask the 'samples' filter before modifying!")
    rownames(x$geno) <- value
  } else if (is_varkit_geno(x)) {
    rownames(x) <- value
  } else {
    stop("`samples()<-` only supports 'varkit' & 'varkit_geno' objects!")
  }

  x
}

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
  samples.varkit_geno(x$geno)
}

##' @export
samples.varkit_header <- function(x) {
  x$col_names[!tolower(x$col_names) %in% .fixed_columns()]
}

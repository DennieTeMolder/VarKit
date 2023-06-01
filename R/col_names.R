##' @export
col_names <- function(x) {
  UseMethod("col_names", x)
}

##' @export
col_names.varkit <- function(x) {
  c(colnames(x$site), samples.varkit(x))
}

col_names.varkit_header <- function(x) {
  x$col_names
}

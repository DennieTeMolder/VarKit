##' @export
meta <- function(x) {
  UseMethod("meta", x)
}

##' @export
meta.varkit_header <- function(x) {
  x$meta
}

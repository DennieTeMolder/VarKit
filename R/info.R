##' @export
info <- function(x) {
  UseMethod("info", x)
}

##' @export
info.varkit <- function(x) {
  dplyr::slice(x$info, x$use_sites)
}

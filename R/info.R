##' @export
fixed <- function(x) {
  UseMethod("fixed", x)
}

##' @export
fixed.varkit <- function(x) {
  dplyr::slice(x$fixed, x$use_sites)
}

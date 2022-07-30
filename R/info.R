##' @export
fixed <- function(x) {
  UseMethod("fixed", x)
}

##' @export
fixed.varkit <- function(x) {
  x$fixed[x$use_site, ]
}

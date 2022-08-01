##' @export
unmask_all <- function(x) {
  UseMethod("unmask_all", x)
}

##' @export
unmask_all.varkit <- function(x) {
  x$use_site <- rep(TRUE, ncol(x$geno))
  x
}

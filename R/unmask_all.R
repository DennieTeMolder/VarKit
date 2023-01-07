##' @export
unmask_all <- function(x, reset_filter = FALSE) {
  UseMethod("unmask_all", x)
}

##' @export
unmask_all.varkit <- function(x, reset_filter = FALSE) {
  x$use_site <- rep(TRUE, ncol(x$geno))
  if (reset_filter)
    x$site$filter <- "PASS"
  x
}

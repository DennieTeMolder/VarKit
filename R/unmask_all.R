##' @export
unmask_all <- function(x, reset_filter = FALSE) {
  UseMethod("unmask_all", x)
}

##' @export
unmask_all.varkit <- function(x, reset_filter = FALSE) {
  .stopifnotsingle(reset_filter, what = "logical")
  if (reset_filter)
    x$site$filter <- "PASS"
  rd_reset(x, indices = "sites")
}

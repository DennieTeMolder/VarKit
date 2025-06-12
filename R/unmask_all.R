##' @export
unmask_all <- function(x, reset_filter = FALSE) {
  UseMethod("unmask_all", x)
}

##' @export
unmask_all.varkit <- function(x, reset_filter = FALSE) {
  drm_deselect(x, "samples", "sites")
  if (reset_filter)
    x$site$filter <- "PASS"
  x
}

##' @export
export_mask <- function(x) {
  UseMethod("export_mask", x)
}

##' @export
export_mask.varkit <- function(x) {
  x$use_site
}

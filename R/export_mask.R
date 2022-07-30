##' @export
export_mask <- function(x) {
  UseMethod("export_mask", x)
}

##' @export
export_mask.varkit <- function(x) {
  list(use_site = x$use_site, use_sample = x$use_sample)
}

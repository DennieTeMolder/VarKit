##' @export
export_mask <- function(x) {
  UseMethod("export_mask", x)
}

##' @export
export_mask.varkit <- function(x) {
  list(
    samples = .drm_if_else(x, node = "samples"),
    sites = .drm_if_else(x, node = "sites")
  )
}

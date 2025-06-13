##' @export
export_mask <- function(x) {
  UseMethod("export_mask", x)
}

##' @export
export_mask.varkit <- function(x) {
  list(
    samples = drm_ifelse(x, node = "samples"),
    sites = drm_ifelse(x, node = "sites")
  )
}

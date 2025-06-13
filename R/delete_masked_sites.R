##' @export
delete_masked_sites <- function(x) {
  UseMethod("delete_masked_sites", x)
}

##' @export
delete_masked_sites.varkit <- function(x) {
  lifecycle::deprecate_warn("v0.9.0", "delete_masked_sites()", I("delete_masked(sites = TRUE, samples = FALSE)"))
  delete_masked.varkit(x, samples = FALSE, sites = TRUE)
}

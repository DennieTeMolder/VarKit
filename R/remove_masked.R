##' @export
remove_masked <- function(x) {
  lifecycle::deprecate_warn("v0.8.2", "remove_masked()", "delete_masked_sites()")
  delete_masked_sites(x)
}

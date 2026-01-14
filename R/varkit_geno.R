##' @export
varkit_geno <- function(x = NULL) {
  as_mtrx(x, dimnames = c("samples", "sites"))
}

##' @export
varkit_geno <- function(x) {
  UseMethod("varkit_geno", x)
}

##' @export
varkit_geno.data.frame <- function(x) {
  varkit_geno.matrix(t(as.matrix(x)))
}

##' @export
varkit_geno.matrix <- function(x) {
  x <- mtrx(x, dim_names = c("samples", "sites"))
  class(x) <- c("varkit_geno", setdiff(class(x), "varkit_geno"))
  x
}

##' @export
is_varkit_geno <- function(x) {
  inherits(x, "varkit_geno")
}

##' @export
`[.varkit_geno` <- function(x, ...) {
  res <- NextMethod()
  if (is.matrix(res))
    class(res) <- class(x)
  res
}

##' @export
t.varkit_geno <- function(x) {
  t(unclass(x))
}

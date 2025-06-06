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
  x <- as_mtrx(x, dimnames = c("samples", "sites"))
  class(x) <- c("varkit_geno", setdiff(class(x), "varkit_geno"))
  x
}

##' @export
is_varkit_geno <- function(x) {
  inherits(x, "varkit_geno")
}

##' @export
`[.varkit_geno` <- function(x, i, j, drop = FALSE, ...) {
  class(x) <- setdiff(class(x), "varkit_geno")
  res <- x[i, j, drop = drop]
  if (is.matrix(res))
    class(res) <- c("varkit_geno", class(res))
  res
}

##' @export
t.varkit_geno <- function(x) {
  t(unclass(x))
}

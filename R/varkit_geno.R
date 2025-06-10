##' @export
varkit_geno <- function(x = NULL) {
  UseMethod("varkit_geno", x)
}

##' @export
varkit_geno.data.frame <- function(x) {
  varkit_geno.default(t(as.matrix(x)))
}

##' @export
varkit_geno.default <- function(x = NULL) {
  x <- as_mtrx(
    x = if (is.null(x)) logical() else x,
    dimnames = c("samples", "sites")
  )
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

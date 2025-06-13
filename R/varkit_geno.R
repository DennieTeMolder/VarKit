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
  add_class <- c("varkit_geno", "mtrx")
  structure(
    as_mtrx(x, dimnames = c("samples", "sites")),
    class = c(add_class, if (!is.null(x)) setdiff(class(x), add_class))
  )
}

##' @export
is_varkit_geno <- function(x) {
  inherits(x, "varkit_geno")
}

##' @export
`[.varkit_geno` <- function(x, i, j, drop = FALSE, ...) {
  class(x) <- setdiff(class(x), "varkit_geno")
  res <- NextMethod("[")
  if (is.matrix(res))
    class(res) <- c("varkit_geno", class(res))
  res
}

##' @export
t.varkit_geno <- function(x) {
  class(x) <- setdiff(class(x), "varkit_geno")
  t.default(x)
}

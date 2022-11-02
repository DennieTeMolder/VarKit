##' @export
apply_sites <- function(x, FUN, ..., simplify = TRUE) {
  UseMethod("apply_sites", x)
}

##' @export
apply_sites.varkit_geno <- function(x, FUN, ..., simplify = TRUE) {
  res <- apply(x, MARGIN = 2, FUN = FUN, ..., simplify = simplify)
  if (is.matrix(res)) {
    if (all(dim(res) == dim(x)))
      return(varkit_geno(res))
  } else if (nrow(x) == 1 && is.vector(res) && !is.list(res)) {
    res <- matrix(res, nrow = 1, dimnames = dimnames(x))
    return(varkit_geno(res))
  }
  res
}

##' @export
apply_sites.varkit <- function(x, FUN, ..., simplify = TRUE) {
  apply_sites(geno(x), FUN = FUN, ... , simplify = simplify)
}

##' @export
apply_samples <- function(x, FUN, ..., simplify = TRUE) {
  UseMethod("apply_samples", x)
}

##' @export
apply_samples.varkit_geno <- function(x, FUN, ..., simplify = TRUE) {
  res <- apply(x, MARGIN = 1, FUN = FUN, ..., simplify = simplify)
  # The apply transposes the matrix, undo this if a matrix is returned
  if (is.matrix(res)) {
    res <- t(res)
    if (all(dim(res) == dim(x)))
      return(varkit_geno(res))
  } else if (nrow(x) == 1 && is.vector(res) && !is.list(res)) {
    res <- matrix(res, nrow = 1, dimnames = dimnames(x))
    return(varkit_geno(res))
  }
res
}

##' @export
apply_samples.varkit <- function(x, FUN, ..., simplify = TRUE) {
  apply_samples(geno(x), FUN = FUN, ... , simplify = simplify)
}

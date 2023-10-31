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
apply_index <- function(x, FUN, ...) {
  UseMethod("apply_index", x)
}

##' @export
apply_index.varkit_index <- function(x, FUN, ..., FROM.START = TRUE, .progress = TRUE) {
  if (FROM.START)
    seek.varkit_index(x, 1)
  purrr::map(seq_index.varkit_index(x), function(i, ...) {
    FUN(suppressMessages(next_chunk.varkit_index(x)), ...)
  }, ..., .progress = .progress)
}

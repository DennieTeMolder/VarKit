##' @export
apply_sites <- function(x, FUN, ..., simplify = TRUE) {
  UseMethod("apply_sites", x)
}

##' @export
apply_sites.varkit_geno <- function(x, FUN, ..., simplify = TRUE) {
  dims <- dim(x)
  x <- apply(x, MARGIN = 2, FUN = FUN, ..., simplify = simplify)
  if (inherits(x, "matrix") && all(dim(x) == dims))
    return(varkit_geno(x))
  x
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
  dims <- dim(x)
  x <- apply(x, MARGIN = 1, FUN = FUN, ..., simplify = simplify)
  # The apply transposes the matrix, undo this if a matrix is returned
  if (inherits(x, "matrix")) {
    x <- t(x)
    if (all(dim(x) == dims))
      return(varkit_geno(x))
  }
  x
}

##' @export
apply_samples.varkit <- function(x, FUN, ..., simplify = TRUE) {
  apply_samples(geno(x), FUN = FUN, ... , simplify = simplify)
}

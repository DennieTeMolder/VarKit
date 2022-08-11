##' @export
apply_sites <- function(x, FUN, ..., simplify = TRUE) {
  UseMethod("apply_sites", x)
}

##' @export
apply_sites.varkit_geno <- function(x, FUN, ..., simplify = TRUE) {
  apply(x, MARGIN = 2, FUN = FUN, ..., simplify = simplify)
}

##' @export
apply_sites.varkit <- function(x, FUN, ..., simplify = TRUE) {
  apply_sites.varkit_geno(geno(x), FUN = FUN, ... , simplify = simplify)
}

##' @export
apply_samples <- function(x, FUN, ..., simplify = TRUE) {
  UseMethod("apply_samples", x)
}

##' @export
apply_samples.varkit_geno <- function(x, FUN, ..., simplify = TRUE) {
  apply(x, MARGIN = 1, FUN = FUN, ..., simplify = simplify)
}

##' @export
apply_samples.varkit <- function(x, FUN, ..., simplify = TRUE) {
  apply_samples.varkit_geno(geno(x), FUN = FUN, ... , simplify = simplify)
}

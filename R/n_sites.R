##' @export
n_sites <- function(x, ...) {
  UseMethod("n_sites", x)
}

##' @export
n_sites.varkit <- function(x, ..., mask = TRUE) {
  .stopifnotsingle(mask, what = "logical")
  if (mask) {
    sum(.indices(x)$sites)
  } else {
    length(.indices(x)$sites)
  }
}

##' @export
n_sites.varkit_index <- function(x, ...) {
  sum(x[["n_lines"]])
}

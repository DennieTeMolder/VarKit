##' @export
n_sites <- function(x, ...) {
  UseMethod("n_sites", x)
}

##' @export
n_sites.varkit <- function(x, ..., mask = TRUE) {
  rlang::check_dots_used()
  .stopifnotsingle(mask, what = "logical")
  if (mask) {
    sum(.indices(x)$sites)
  } else {
    length(.indices(x)$sites)
  }
}

##' @export
n_sites.varkit_index <- function(x, ...) {
  rlang::check_dots_used()
  sum(x[["n_lines"]])
}

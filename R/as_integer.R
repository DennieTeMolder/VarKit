# Version of as.integer() that preserves arrays and warns if there is a loss of precision
##' @export
as_integer <- function(x, check = TRUE) {
  if (is.integer(x))
    return(x)

  # Check if precision is lost when going from double to integer
  if (check && !is.logical(x)) {
    if (!is.numeric(x))
      storage.mode(x) <- "double"
    if (!rlang::is_integerish(x))
      warning("Loss of precision occurred when converting to integer.")
  }

  storage.mode(x) <- "integer"
  x
}

# Version of as.integer() that warns if there is a loss of precision
##' @export
as_integer <- function(x) {
  if (is.integer(x))
    return(x)

  if (is.array(x)) {
    result <- x
    storage.mode(result) <- "integer"
  } else if (!is.vector(x)) {
    stop("'x' should either be a vector or an array!")
  } else {
    result <- as.integer(x)
  }

  if (any(abs(x - result) > .Machine$double.eps^0.5, na.rm = TRUE))
    warning("Loss of precision occurred when converting 'double' to 'integer'.")
  result
}

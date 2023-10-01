##' @export
set_origin <- function(x, origin) {
  if (is.character(origin) && length(origin) == 1) {
    sha1 <- origin
  } else {
    sha1 <- digest::sha1(origin)
  }
  attr(x, "varkit_origin") <- sha1
  x
}

##' @export
get_origin <- function(x, ensure = FALSE) {
  origin <- attr(x, "varkit_origin", exact = TRUE)
  if (ensure && is.null(origin))
    stop("Argument '", substitute(x), "' has no 'varkit_origin' attribute!")
  origin
}

##' @export
originates_from <- function(x, origin) {
  get_origin(x, ensure = TRUE) == digest::sha1(origin)
}

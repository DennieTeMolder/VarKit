##' @export
set_origin <- function(x, origin) {
  if (is.character(origin) && length(origin) == 1L) {
    sha1 <- origin
  } else {
    sha1 <- digest::sha1(origin)
  }
  if (inherits(x, "igraph")) {
    .ensure_igraph()
    igraph::graph_attr(x, "varkit_origin") <- sha1
  } else {
    attr(x, "varkit_origin") <- sha1
  }
  x
}

##' @export
get_origin <- function(x, ensure = FALSE) {
  if (inherits(x, "igraph")) {
    .ensure_igraph()
    origin <- igraph::graph_attr(x, "varkit_origin")
  } else {
    origin <- attr(x, "varkit_origin", exact = TRUE)
  }
  if (ensure && is.null(origin))
    stop("Argument '", substitute(x), "' has no 'varkit_origin' attribute!")
  origin
}

##' @export
originates_from <- function(x, origin) {
  get_origin(x, ensure = TRUE) == digest::sha1(origin)
}

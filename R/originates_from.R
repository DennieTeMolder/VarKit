##' @export
set_origin <- function(x, origin) {
  hash <- varkit_hash(origin)
  if (inherits(x, "igraph")) {
    .ensure_igraph()
    igraph::graph_attr(x, "varkit_origin") <- hash
  } else {
    attr(x, "varkit_origin") <- hash
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
  any(get_origin(x, ensure = TRUE) == varkit_hash(origin))
}

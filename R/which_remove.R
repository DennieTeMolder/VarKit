##' @export
which_remove <- function(x) {
  UseMethod("which_remove", x)
}

##' @export
which_remove.varkit_linkage_graph <- function(x) {
  deg <- igraph::degree(x, mode = "in", loops = FALSE)
  if (any(deg >= 2))
    stop("The linkage graph appears to be corrupted!")
  as.integer(names(deg)[deg == 1])
}

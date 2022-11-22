##' @export
get_types <- function(x, check = TRUE) {
  UseMethod("get_types", x)
}

##' @export
get_types.link_list <- function(x, check = TRUE) {
  if (check)
    .check_link_list(x)
  types <- rep("unlinked", length(x))
  types[sapply(x, function(y) y[1] > 0)] <- "node"
  types[is.na(x)] <- "removed"
  types
}

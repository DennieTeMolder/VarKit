##' @export
is_unlinked <- function(x) {
  UseMethod("is_unlinked", x)
}

##' @export
is_unlinked.link_list <- function(x) {
  my_fun <- function(values) length(values) == 1 && isTRUE(values == 0)
  sapply(x, my_fun, USE.NAMES = FALSE)
}

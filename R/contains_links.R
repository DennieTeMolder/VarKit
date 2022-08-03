##' @export
contains_link <- function(x) {
  UseMethod("contains_link", x)
}

##' @export
contains_link.link_list <- function(x) {
  my_fun <- function(values) length(values) > 1 || isTRUE(values > 0)
  sapply(x, my_fun, USE.NAMES = FALSE)
}

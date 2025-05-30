##' @export
is_open <- function(x) {
  UseMethod("is_open", x)
}

##' @export
is_open.connection <- function(x) {
  tryCatch(isOpen(x), error = function(e) FALSE)
}

##' @export
is_open.varkit_index <- function(x) {
  if (is.null(.env(x)$con) || is.na(.env(x)$con))
    return(FALSE)
  is_open.connection(.env(x)$con)
}

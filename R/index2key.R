##'@export
index2key <- function(x, idx) {
  UseMethod("index2key", x)
}

##' @export
index2key.link_list <- function(x, idx) {
  stopifnot(is.numeric(idx))
  trans <- attr(x, "translation")
  if (!is.null(trans))
    return(trans[idx])
  idx
}

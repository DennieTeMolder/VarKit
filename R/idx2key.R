##'@export
idx2key <- function(x, idx) {
  UseMethod("idx2key", x)
}

##' @export
idx2key.link_list <- function(x, idx) {
  stopifnot(is.numeric(idx))
  trans <- attr(x, "translation")
  if (!is.null(trans))
    return(trans[idx])
  idx
}

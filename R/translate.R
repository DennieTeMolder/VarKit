##'@export
translate <- function(x, entries) {
  UseMethod("translate", x)
}

##' @export
translate.link_list <- function(x, entries) {
  trans <- attr(x, "translation")
  if (!is.null(trans))
    return(trans[entries])
  entries
}

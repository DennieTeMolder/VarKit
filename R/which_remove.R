##' @export
which_remove <- function(x, translate_out = TRUE) {
  UseMethod("which_remove", x)
}

##' @export
which_remove.link_list <- function(x, translate_out = TRUE) {
  all_values <- unlist(x)
  links <- .linked_only(all_values)
  links <- sort(links)
  if (translate_out)
    return(index2key(x, links))
  links
}

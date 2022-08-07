##' @export
which_remove <- function(x, translate = TRUE) {
  UseMethod("which_remove", x)
}

##' @export
which_remove.link_list <- function(x, translate = TRUE) {
  all_values <- unlist(x)
  links <- .linked_only(all_values)
  links <- sort(links)
  if (translate)
    return(translate(x, links))
  links
}

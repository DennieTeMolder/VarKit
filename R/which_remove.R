##' @export
which_remove <- function(ll, translate = TRUE) {
  UseMethod("which_remove", ll)
}

##' @export
which_remove.link_list <- function(ll, translate = TRUE) {
  all_values <- unlist(ll)
  links <- .linked_only(all_values)
  links <- sort(links)
  if (translate)
    return(translate(ll, links))
  links
}

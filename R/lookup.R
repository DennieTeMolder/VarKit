##' @export
lookup <- function(ll, idx, translate = TRUE) {
  UseMethod("lookup", ll)
}

##' @export
lookup.link_list <- function(ll, idx, translate = TRUE) {
  stopifnot(is.numeric(idx))
  if (any(idx > length(ll)))
    stop("Some indexes (idx) are out of range!")
  values <- unlist(ll[idx])
  links <- .linked_only(values, allow_na = FALSE)
  if (translate)
    return(translate(ll, links))
  links
}

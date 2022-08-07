##' @export
lookup <- function(x, idx, translate = TRUE) {
  UseMethod("lookup", x)
}

##' @export
lookup.link_list <- function(x, idx, translate = TRUE) {
  stopifnot(is.numeric(idx))
  if (any(idx > length(x)))
    stop("Some indexes (idx) are out of range!")
  values <- unlist(x[idx])
  links <- .linked_only(values, allow_na = FALSE)
  if (translate)
    return(translate(x, links))
  links
}

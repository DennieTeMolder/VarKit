##' @export
lookup <- function(x, keys, translate = "both") {
  UseMethod("lookup", x)
}

##' @export
lookup.link_list <- function(x, keys, translate = "both") {
  stopifnot(is.vector(keys))
  stopifnot(is.character(translate))
  stopifnot(translate %in% c("in", "out", "both", "none"))

  translate_in <- translate %in% c("in", "both")
  translate_out <- translate %in% c("out", "both")

  if (translate_in) {
    idx <- key2idx(x, keys)
  } else {
    stopifnot(is.numeric(keys))
    if (any(keys > length(x)))
      stop("Some keys are out of range!")
    idx <- keys
  }

  values <- unlist(x[idx])
  links <- .linked_only(values, allow_na = FALSE)

  if (translate_out)
    return(idx2key(x, links))
  links
}

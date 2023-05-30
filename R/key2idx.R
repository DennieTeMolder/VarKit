##'@export
key2idx <- function(x, keys) {
  UseMethod("key2idx", x)
}

##' @export
key2idx.link_list <- function(x, keys) {
  stopifnot(is.vector(keys))

  trans <- attr(x, "translation")

  if (is.null(trans))
    stop("Link list 'x' has no translation layer!")

  matches <- match(keys, trans)

  if (any(is.na(matches)))
    stop("Not all 'keys' were found in the translation layer!")

  matches
}

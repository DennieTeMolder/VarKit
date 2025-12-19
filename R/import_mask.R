##' @export
import_mask <- function(x, mask, add = FALSE) {
  UseMethod("import_mask", x)
}

##' @export
import_mask.varkit <- function(x, mask, add = FALSE) {
  validate_varkit(x)
  stopifnot(is.list(mask), rlang::is_named(mask))
  .stopifnotsingle(add, what = "logical")

  mask <- purrr::imap(mask, function(curr, idx) {
    if (!(is.vector(curr) && is.logical(curr)))
      stop("Expected a vector of type logical!")

    idx <- .indices(x)[[idx]]
    if (length(curr) != length(idx))
      stop("Expected length ", length(idx),
           ", but got ", length(curr), "!")

    if (add) {
      curr & idx
    } else {
      curr
    }
  })

  .indices(x)[names(mask)] <- mask
}

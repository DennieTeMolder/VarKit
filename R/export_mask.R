##' @export
export_mask <- function(x) {
  UseMethod("export_mask", x)
}

##' @export
export_mask.varkit <- function(x) {
  result <- linked_data_indices_get(x)
  slots <- rep(list(""), length(result))
  names(slots) <- names(result)
  new_linked_data(result, slots = slots, indices = result)
}

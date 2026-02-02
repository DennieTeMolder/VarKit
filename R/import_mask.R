##' @export
import_mask <- function(x, mask, add = FALSE) {
  UseMethod("import_mask", x)
}

##' @export
import_mask.varkit <- function(x, mask, add = FALSE) {
  linked_data_indices_set(x, indices = linked_data_unclass(mask), add = add)
}

##' @export
delete_masked_sites <- function(x) {
  UseMethod("delete_masked_sites", x)
}

##' @export
delete_masked_sites.varkit <- function(x) {
  validate_varkit(x)

  # Reset all other indices so they do not get applied
  indices <- .indices(x)
  reset_ind <- setdiff(names(indices), "sites")
  x <- linked_data_reset(x, indices = reset_ind)

  # Apply the mask
  x <- x[]

  # Restore the unused indices
  .indices(x) <- indices[reset_ind]
  x
}

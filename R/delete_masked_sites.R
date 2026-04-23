##' @export
delete_masked_sites <- function(x) {
  UseMethod("delete_masked_sites", x)
}

##' @export
delete_masked_sites.varkit <- function(x) {
  validate_varkit(x)

  # Store 'related_data' specific attributes to restore later
  slots <- attr(x, "slots")
  indices <- attr(x, "indices")
  keys <- attr(x, "keys")

  # Short-circuit if there are no masked sites
  if (all(indices$sites))
    return(x)

  # Clear all other indices from the slots so they are not shrunken
  attr(x, "slots") <- lapply(slots, function(slot) {
    slot[slot != "sites"] <- ""
    slot
  })

  # Shrink to delete masked values
  x <- rd_shrink(x)

  # Transfer modified indices
  indices[["sites"]] <- getElement(attr(x, "indices"), "sites")
  keys[["sites"]] <- getElement(attr(x, "keys"), "sites")

  # Restore
  attr(x, "slots") <- slots
  attr(x, "indices") <- indices
  attr(x, "keys") <- keys

  x
}

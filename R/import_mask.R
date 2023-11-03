##' @export
import_mask <- function(x, mask, add = FALSE) {
  UseMethod("import_mask", x)
}

##' @export
import_mask.varkit <- function(x, mask, add = FALSE) {
  # Checks
  stopifnot(is.logical(mask))
  if (length(mask) != n_sites.varkit(x, mask = FALSE))
    stop("The number of sites in 'mask' doesn't match with 'x'!")

  if (add)
    mask <- x$use_site & mask
  if (!any(mask))
    stop("The imported mask will hide all sites!")

  # Apply mask
  x$use_site <- mask
  x
}

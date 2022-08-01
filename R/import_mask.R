##' @export
import_mask <- function(x, mask) {
  UseMethod("import_mask", x)
}

##' @export
import_mask.varkit <- function(x, mask) {
  # Checks
  stopifnot(is.logical(mask))
  if (length(mask) != ncol(x$geno))
    stop("The number of sites in 'mask' doesn't match with 'x'!")
  # Apply mask
  x$use_site <- mask
  x
}

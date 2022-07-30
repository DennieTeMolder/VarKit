##' @export
import_mask <- function(x, mask, mask_sites = TRUE, mask_samples = TRUE) {
  UseMethod("import_mask", x)
}

##' @export
import_mask.varkit <- function(x, mask, mask_sites = TRUE, mask_samples = TRUE) {
  # Checks
  if (!all(c("use_site", "use_sample") %in% names(mask)))
    stop("The provided 'mask' is not compatible with VarKit!")
  if (length(mask$use_site) != ncol(x$geno))
    stop("The number of sites in 'mask' doesn't match with 'x'!")
  if (length(mask$use_sample) != length(x$samples))
    stop("The number of samples in 'mask' doesn't match with 'x'!")
  # Apply mask
  if (mask_sites)
    x$use_site <- mask$use_site
  if (mask_samples)
    x$use_sample <- mask$use_sample
  x
}

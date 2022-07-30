##' @export
unmask_all <- function(x, unmask_sites = TRUE, unmask_samples = TRUE) {
  UseMethod("unmask_all", x)
}

##' @export
unmask_all.varkit <- function(x, unmask_sites = TRUE, unmask_samples = TRUE) {
  stopifnot(unmask_sites || unmask_samples)
  if (unmask_sites)
    x$use_site <- rep(TRUE, ncol(x$geno))
  if (unmask_samples)
    x$use_sample <- rep(TRUE, length(x$samples))
  x
}

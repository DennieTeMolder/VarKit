##' @export
find_linked_sites <- function(x,
                              cutoff = .9,
                              window = ifelse(by_chrom, 1e5, -1),
                              by_chrom = TRUE,
                              cores = 1,
                              debug = FALSE) {
  UseMethod("find_linked_sites", x)
}

##' @export
##' @import foreach
find_linked_sites.varkit <- function(x,
                                     cutoff = .9,
                                     window = ifelse(by_chrom, 1e5, -1),
                                     by_chrom = TRUE,
                                     cores = 1,
                                     debug = FALSE) {
  .stopifnotsingle(cutoff, class = "numeric")
  .stopifnotsingle(by_chrom, class = "logical")
  .stopifnotsingle(window, class = "numeric")
  if (!by_chrom && !.window_disabled(window)) {
    warning("The window option is only supported when `by_chrom = TRUE`, disabling window...")
    window <- -1
  }

  .compare_sites(x, FUN = find_linked_cols, by_chrom = by_chrom, cores = cores,
                 debug = debug, cutoff = cutoff, window = window)
}

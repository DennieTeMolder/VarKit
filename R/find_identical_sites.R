##' @export
find_identical_sites <- function(x,
                                 tolerance = .01,
                                 window = -1,
                                 by_chrom = TRUE,
                                 cores = 1,
                                 debug = FALSE) {
  UseMethod("find_identical_sites", x)
}

##' @export
##' @import foreach
find_identical_sites.varkit <- function(x,
                                        tolerance = .01,
                                        window = -1,
                                        by_chrom = TRUE,
                                        cores = 1,
                                        debug = FALSE) {
  .stopifnotsingle(tolerance, class = "numeric")
  .stopifnotsingle(by_chrom, class = "logical")
  .stopifnotsingle(window, class = "numeric")
  if (!by_chrom && !.window_disabled(window)) {
    warning("The window option is only supported when `by_chrom = TRUE`, disabling window...")
    window <- -1
  }

  .compare_sites(x, FUN = find_identical_cols, by_chrom = by_chrom, cores = cores,
                 debug = debug, tolerance = tolerance, window = window)
}

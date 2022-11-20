##' @export
find_identical_cols <- function(x,
                                pos = NULL,
                                tolerance = .01,
                                window = 0,
                                llist = NULL) {
  .compare_cols(x, FUN = .compare_absolute, pos = pos, window = window, llist = llist, tolerance = tolerance)
}

.compare_absolute <- function(a, b, tolerance) {
  mean_diff <- colMeans(abs(b - a), na.rm = TRUE)
  mean_diff <= tolerance & !is.na(mean_diff)
}

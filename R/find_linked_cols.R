##' @export
find_linked_cols <- function(mat,
                             cutoff = 0.9,
                             windows = -1,
                             cols_each = 2000,
                             return_r2 = FALSE,
                             cores = 1,
                             min_ncol = 20
                             ) {
  stopifnot(is.matrix(mat), is.numeric(mat))
  .stopifnotsingle(cutoff, what = is.numeric)
  .stopifnotsingle(windows, what = is.numeric)
  .stopifnotsingle(cols_each, what = is.numeric)
  .stopifnotsingle(return_r2, what = "logical")
  .stopifnotsingle(cores, what = is.numeric)
  .stopifnotsingle(min_ncol, what = is.numeric)

  # Ensure the matrix has col names so it can be reconstructed
  if (is.null(colnames(mat)))
    colnames(mat) <- seq_len(ncol(mat))

  # Split the matrix by COLS_EACH and generate the comparisons according to WINDOWs
  mat_list <- splitm(mat, margin = 2, size = cols_each, by_na_pattern = TRUE, min_size = min_ncol)
  comps <- compute_comparisons(mat_list, windows = windows)

  res <- .r2_cutoff_parallel(mat_list, comps = comps, cutoff = cutoff, return_r2 = return_r2, cores = cores)
  do.call(rbind, res)
}

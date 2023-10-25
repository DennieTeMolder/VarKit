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
  stopifnot(is.numeric(cutoff), length(cutoff) == 1)
  stopifnot(is.numeric(cols_each), length(cols_each) == 1)
  stopifnot(is.numeric(windows), length(windows) == 1)
  stopifnot(is.numeric(min_ncol), length(min_ncol) == 1)

  # Ensure the matrix has col names so it can be reconstructed
  if (is.null(colnames(mat)))
    colnames(mat) <- seq_len(ncol(mat))

  # Split the matrix by COLS_EACH and generate the comparisons according to WINDOWs
  mat_list <- splitm(mat, margin = 2, size = cols_each, by_na_pattern = TRUE, min_size = min_ncol)
  comps <- compute_comparisons(mat_list, windows = windows)

  res <- .r2_cutoff_parallel(mat_list, comps = comps, cutoff = cutoff, return_r2 = return_r2, cores = cores)
  do.call(rbind, res)
}

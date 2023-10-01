##' @export
find_linked_cols <- function(mat,
                             cutoff = 0.9,
                             cols_each = 2000,
                             window = -1,
                             return_r2 = FALSE,
                             cores = 1,
                             min_ncol = 20
                             ) {
  stopifnot(is.matrix(mat), is.numeric(mat))
  stopifnot(is.numeric(cutoff), length(cutoff) == 1)
  stopifnot(is.numeric(cols_each), length(cols_each) == 1)
  stopifnot(is.numeric(window), length(window) == 1)
  stopifnot(is.numeric(min_ncol), length(min_ncol) == 1)

  # Ensure the matrix has col names so it can be reconstructed
  if (is.null(colnames(mat)))
    colnames(mat) <- seq_len(ncol(mat))

  # Split the matrix by COLS_EACH and generate the comparisons according to WINDOW
  mat_list <- splitm(mat, margin = 2, size = cols_each, by_na_pattern = TRUE, min_size = min_ncol)
  comps <- .generate_comparisons(mat_list, window = window)

  res <- .r2_cutoff_parallel(comps$x, comps$y, cutoff = cutoff, return_r2 = return_r2, cores = cores)
  do.call(rbind, res)
}

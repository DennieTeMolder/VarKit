##' @export
separate_mat_to_3d <- function(mat, delim = ",", ptype = NULL) {
  stopifnot(is.matrix(mat), is.character(mat))

  # Find the maximum number of delimiters and split into an pre-allocated matrix
  delim <- stringr::fixed(delim)
  n_max <- max(stringr::str_count(mat, delim), na.rm = TRUE) + 1L
  splits <- stringr::str_split_fixed(mat, delim, n = n_max)

  # Optional type conversion
  if (!is.null(ptype))
    storage.mode(splits) <- typeof(ptype)

  # Convert to 3d array and restore attributes
  dim(splits) <- c(dim(mat), n_max)
  if (!is.null(dimnames(mat)))
    dimnames(splits) <- c(dimnames(mat), list(NULL))

  splits
}

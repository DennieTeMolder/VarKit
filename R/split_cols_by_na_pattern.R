##' @export
split_cols_by_na_pattern <- function(mat, na.rm = FALSE) {
  stopifnot(is.matrix(mat))
  is_na <- is.na(mat)
  dimnames(is_na) <- NULL
  if (!any(is_na))
    return(list(idx = seq_len(ncols(mat)), splits = mat))

  na_pattern <- is_na
  storage.mode(na_pattern) <- "integer"
  storage.mode(na_pattern) <- "character"
  na_pattern <- factor(do.call(paste0, asplit(na_pattern, 1)))
  na_pattern <- split(seq_along(na_pattern), na_pattern)

  if (na.rm) {
    splits <- lapply(na_pattern, function(ind) {
      keep_rows <- !is_na[, ind[1]]
      mat[keep_rows, ind, drop = FALSE]
    })
  } else {
    splits <- lapply(na_pattern, function(ind) {
      mat[, ind, drop = FALSE]
    })
  }
  splits <- lapply(splits, function(split) {
    class(split) <- class(mat)
    split
  })

  list(idx = na_pattern, splits = splits)
}

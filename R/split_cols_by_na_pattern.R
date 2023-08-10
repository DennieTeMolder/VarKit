##' @export
split_cols_by_na_pattern <- function(mat, min_ncol = 1) {
  stopifnot(is.matrix(mat))
  is_na <- is.na(mat)
  dimnames(is_na) <- NULL

  dummy_result <- function(name) {
    idx <- list(seq_len(ncol(mat)))
    splits <- list(mat)
    names(idx) <- names(splits) <- name
    list(idx = idx, splits = splits)
  }

  if (!any(is_na))
    return(dummy_result(.collapse(rep(0, nrow(mat)), "")))

  na_pattern <- is_na
  storage.mode(na_pattern) <- "integer"
  storage.mode(na_pattern) <- "character"
  na_pattern <- factor(do.call(paste0, asplit(na_pattern, 1)))

  if (min_ncol > 1) {
    na_pattern <- forcats::fct_lump_min(na_pattern, min = min_ncol, other_level = "below_min_ncol")
    if (length(levels(na_pattern)) == 1)
      return(dummy_result("below_min_ncol"))
  }

  na_pattern <- split(seq_along(na_pattern), na_pattern)

  splits <- lapply(na_pattern, function(ind) {
    split <- mat[, ind, drop = FALSE]
    class(split) <- class(mat)
    split
  })

  list(idx = na_pattern, splits = splits)
}

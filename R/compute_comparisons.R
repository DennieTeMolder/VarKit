# Intended for use with splitm(by_na_pattern = TRUE)
##' @export
compute_comparisons <- function(x, y = NULL, windows = -1, use = FALSE) {
  .stopifnotsingle(windows, what = is.numeric)
  .stopifnotsingle(use, what = "logical")
  if (!is.null(y) && windows >= 0)
    stop("A positive 'windows' argument produces nonsensical results if 'y' is supplied.")

  seq_x <- seq_along(x)
  seq_y <- if (is.null(y)) seq_x else seq_along(y)
  idxs <- expand.grid(j = seq_y, i = seq_x, KEEP.OUT.ATTRS = FALSE)

  if (is.null(y))
    idxs <- idxs[idxs[[1]] >= idxs[[2]], ]

  if (windows >= 0)
    idxs <- idxs[idxs[[1]] - idxs[[2]] <= windows, ]

  rownames(idxs) <- NULL
  idx <- tibble::as_tibble(idxs[, 2:1])

  if (!use)
    return(idx)

  # Indices of impure matrices, i.e. with variable NA postions on the rows
  x_mixed_idx <- match("below_min_size", names(x), nomatch = 0)
  if (is.null(y)) {
    y_mixed_idx <- x_mixed_idx
  } else {
    y_mixed_idx <- match("below_min_size", names(y), nomatch = 0)
  }

  # Use a different comparison type when an at least one matrix is impure
  idx$use <- ifelse(
    idx$i == x_mixed_idx | idx$j == y_mixed_idx,
    "pairwise.complete.obs",
    "na.or.complete"
  )

  idx
}

##' @export
which_nth_lowest <- function(mat, margin, n = 1, ...) {
  stopifnot(is.matrix(mat))
  .stopifnotsingle(margin, what = is.numeric)
  .stopifnotsingle(n, what = is.numeric)

  margin <- as.integer(margin)
  n <- as.integer(n)

  if (identical(margin, 1L)) {
    rank_fun <- matrixStats::rowRanks
  } else if (identical(margin, 1L)) {
    rank_fun <- matrixStats::colRanks
  } else {
    stop("Unknown value of argument 'margin': ", margin)
  }

  nth_ind <- which(
    rank_fun(mat, ties.method = "first", useNames = FALSE, ...) == n,
    arr.ind = TRUE
  )

  which_nth <- rep(NA_integer_, nrow(nth_ind))
  which_nth[nth_ind[, "row"]] <- nth_ind[, "col"]
  which_nth
}

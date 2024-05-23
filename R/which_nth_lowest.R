##' @export
which_nth_lowest <- function(mat, margin, n = 1, ...) {
  stopifnot(is.matrix(mat))
  stopifnot(length(margin) == 1L)
  stopifnot(is.numeric(n), length(n) == 1L)
  n <- as.integer(n)

  if (margin == 1) {
    rank_fun <- matrixStats::rowRanks
  } else if (margin == 2) {
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

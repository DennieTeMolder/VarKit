# Returns the first non NA value for each row in matrix/df X
##' @export
get_first_non_na_rowwise <- function(x) {
  stopifnot(is.matrix(x) || is.data.frame(x))

  non_na <- x[, 1L, drop = TRUE]
  for (i in seq_len(ncol(x))[-1L]) {
    which_na <- which(is.na(non_na))
    if (length(which_na) == 0L)
      break()
    non_na[which_na] <- x[which_na, i, drop = TRUE]
  }

  non_na
}

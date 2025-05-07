# Returns the first non NA value for each row in matrix/df X
##' @export
get_first_non_na <- function(x, margin = 1) {
  stopifnot(is.matrix(x) || is.data.frame(x))

  if (margin == 1L) {
    non_na <- x[, 1L, drop = TRUE]
    for (i in seq_len(ncol(x))[-1L]) {
      which_na <- which(is.na(non_na))
      if (length(which_na) == 0L)
        break()
      non_na[which_na] <- x[which_na, i, drop = TRUE]
    }
  } else if (margin == 2L) {
    non_na <- x[1L, , drop = TRUE]
    for (i in seq_len(nrow(x))[-1L]) {
      which_na <- which(is.na(non_na))
      if (length(which_na) == 0L)
        break()
      non_na[which_na] <- x[i, which_na, drop = TRUE]
    }
  } else {
    stop("Only 'margin' value of 1 and 2 are supported! Current value: ", margin)
  }

  non_na
}

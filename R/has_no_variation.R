##' @export
has_no_variation <- function(x, margin = 1, na.rm = TRUE) {
  stopifnot(is.numeric(margin), length(margin) == 1, margin %in% 1:2)
  if (is.vector(x))
    return(.has_no_variation.vector(x = x, na.rm = na.rm))
  if (is.matrix(x))
    return(.has_no_variation.matrix(x = x, na.rm = na.rm, margin = margin))
  stop("'x' must be a vector or matrix!")
}

.has_no_variation.vector <- function(x, na.rm) {
  non_na <- x[match(FALSE, is.na(x))]
  if (is.na(non_na))
    return(TRUE)
  if (!na.rm && anyNA(x))
    return(FALSE)
  all(x == non_na, na.rm = TRUE)
}

.has_no_variation.matrix <- function(x, na.rm, margin) {
  if (margin == 2)
    x <- t(x)

  non_na <- x[, 1L, drop = TRUE]
  is_na <- is.na(non_na)
  for (i in seq_len(ncol(x))[-1L]) {
    if (!any(is_na))
      break()
    non_na[is_na] <- x[is_na, i, drop = TRUE]
    is_na <- is.na(non_na)
  }

  all_equal <- matrixStats::rowAlls(x == non_na, na.rm = na.rm)

  if (!na.rm)
    all_equal[is_na] <- TRUE
  all_equal
}

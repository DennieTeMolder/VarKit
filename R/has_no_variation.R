##' @export
has_no_variation <- function(x, na.rm = TRUE, margin = 1) {
  .stopifnotsingle(margin, what = is.numeric)
  stopifnot(margin %in% c(1, 2))
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
  # Obtain the first non-na value for each row/column if it exists
  non_na <- get_first_non_na(x, margin = margin)

  # Expand to an equally sized matrix
  non_na <- matrix(non_na, nrow = nrow(x), ncol = ncol(x), byrow = margin == 2)

  if (margin == 1) {
    all_equal <- matrixStats::rowAlls(x == non_na, na.rm = na.rm)
  } else {
    all_equal <- matrixStats::colAlls(x == non_na, na.rm = na.rm)
  }

  # all() always returns TRUE for a 0/1 length row/column (after NA.RM)
  # NA is only returned when NA.RM is TRUE and the row/column has no FALSE values
  if (margin == 1) {
    all_equal <- matrixStats::rowAlls(x == non_na, na.rm = na.rm)
  } else {
    all_equal <- matrixStats::colAlls(x == non_na, na.rm = na.rm)
  }

  # With NA.RM on, values can only be TRUE/FALSE
  if (na.rm || !anyNA(all_equal))
    return(all_equal)

  # If NAs are present in the output, variation can also occur between NA and non-NA values
  which_na <- which(is.na(all_equal))
  all_equal[which_na] <- is.na(non_na[1L, which_na])
  all_equal
}

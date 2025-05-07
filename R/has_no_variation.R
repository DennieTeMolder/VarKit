##' @export
has_no_variation <- function(x, na.rm = TRUE, margin = 1) {
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
  # Obtain the first non-na value for each row/column if it exists
  non_na <- get_first_non_na(x, margin = margin)

  if (margin == 1) {
    all_equal <- matrixStats::rowAlls(x == non_na, na.rm = na.rm)
  } else {
    all_equal <- matrixStats::colAlls(x == non_na, na.rm = na.rm)
  }

  # Return TRUE if NA.RM is enabled and all values in a row/column are NA
  if (na.rm || !anyNA(all_equal))
    return(all_equal)

  # If NAs are not removed, variation can also occur between NA and non-NA values
  which_na <- which(is.na(all_equal))
  all_equal[which_na] <- is.na(non_na[which_na])
  all_equal
}

##' @export
has_no_variation <- function(x, na_exclude = TRUE, margin = 1) {
  .stopifnotsingle(margin, what = is.numeric)
  stopifnot(margin %in% c(1, 2))
  if (is.vector(x))
    return(.has_no_variation_vector(x = x, na_exclude = na_exclude))
  if (is.matrix(x))
    return(.has_no_variation_matrix(x = x, na_exclude = na_exclude, margin = margin))
  stop("'x' must be a vector or matrix!")
}

.has_no_variation_vector <- function(x, na_exclude) {
  non_na <- x[match(FALSE, is.na(x))]
  if (is.na(non_na))
    return(TRUE)
  if (!na_exclude && .any_na(x))
    return(FALSE)
  all(x == non_na, na.rm = TRUE)
}

.has_no_variation_matrix <- function(x, na_exclude, margin) {
  # Obtain the first non-na value for each row/column if it exists
  non_na <- get_first_non_na(x, margin = margin)

  # Expand to an equally sized matrix
  non_na <- matrix(non_na, nrow = nrow(x), ncol = ncol(x), byrow = margin == 2)

  if (margin == 1) {
    all_equal <- matrixStats::rowAlls(x == non_na, na.rm = na_exclude)
  } else {
    all_equal <- matrixStats::colAlls(x == non_na, na.rm = na_exclude)
  }

  # all() always returns TRUE for a 0/1 length row/column (after NA.RM)
  # NA is only returned when NA.RM is TRUE and the row/column has no FALSE values
  if (margin == 1) {
    all_equal <- matrixStats::rowAlls(x == non_na, na.rm = na_exclude)
  } else {
    all_equal <- matrixStats::colAlls(x == non_na, na.rm = na_exclude)
  }

  # If NA_EXCLUDE is enabled, values can only be TRUE/FALSE
  if (na_exclude || !.any_na(all_equal))
    return(all_equal)

  # If NAs are present in the output, variation can also occur between NA and non-NA values
  which_na <- which(is.na(all_equal))
  all_equal[which_na] <- is.na(non_na[1L, which_na])
  all_equal
}

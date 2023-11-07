##' @export
has_variation <- function(x, na.rm = TRUE, ...) {
  UseMethod("has_variation", x)
}

##' @export
has_variation.double <- function(x, na.rm = TRUE, ...) {
  if (any(is.infinite(x)))
    stop("Infinite values are not allowed!")

  # If all values are NA, min() will give a warning and return Inf
  x_min <- suppressWarnings(min(x, na.rm = TRUE))
  if (x_min == Inf)
    return(FALSE)

  # If we make it here there are at least some non-NA values
  # This means any NAs indicate variation if they are not to be removed
  if (!na.rm && anyNA(x))
    return(TRUE)

  x_min != max(x, na.rm = TRUE)
}

##' @export
has_variation.integer <- has_variation.double

##' @export
has_variation.logical <- function(x, na.rm = TRUE, ...) {
  has_variation.integer(as.integer(x), na.rm = na.rm, ...)
}

##' @export
has_variation.factor <- function(x, na.rm = TRUE, ...) {
  has_variation.integer(as.integer(x), na.rm = na.rm, ...)
}

##' @export
has_variation.character <- function(x, na.rm = TRUE, ...) {
  has_variation.factor(factor(x), na.rm = na.rm, ...)
}


##' @export
has_variation.matrix <- function(x, na.rm = TRUE, ..., margin = 1, use = NULL) {
  stopifnot(is.matrix(x))
  margin <- as.integer(margin)
  stopifnot(margin %in% 1:2)

  # Convert matrix to numeric
  if (is.numeric(x)) {
    if (any(is.infinite(x)))
      stop("Infinite values are not allowed!")
  } else if (is.logical(x)) {
    storage.mode(x) <- "integer"
  } else if (is.character(x)) {
    x <- matrix(as.integer(factor(x)), nrow = nrow(x))
  } else {
    stop("A matrix of type '", typeof(x), "' is not supported!")
  }

  if (margin == 1L) {
    ranges <- matrixStats::rowRanges(x, na.rm = TRUE, useNames = FALSE, row = use)
  } else {
    ranges <- matrixStats::colRanges(x, na.rm = TRUE, useNames = FALSE, col = use)
  }

  # First column is minimum values, second are maximum values
  not_eq <- ranges[, 2L] != ranges[, 1L]

  if (!na.rm) {
    if (margin == 1L) {
      not_eq <- not_eq | matrixStats::rowAnyNAs(x, useNames = FALSE, row = use)
    } else {
      not_eq <- not_eq | matrixStats::colAnyNAs(x, useNames = FALSE, col = use)
    }
  }

  not_eq & ranges[, 1L] != Inf
}

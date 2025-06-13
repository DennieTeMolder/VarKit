# Implements stats::cor(use = "pairwise.complete.obs", method = "pearson") using BLAS
##' @export
pcor_pairwise <- function(x, y = NULL, min_ncol = 20) {
  use_y <- !is.null(y)

  stopifnot(is.matrix(x))
  if (use_y)
    stopifnot(is.matrix(y), nrow(x) == nrow(y))
  .stopifnotsingle(min_ncol, what = is.numeric)

  # Unclass so primitive functions are always used
  x <- unclass(x)
  if (use_y)
    y <- unclass(y)

  # If there are no NAs we can save ourselves a lot of hassle
  if (!anyNA(x) && !(use_y && anyNA(y)))
    return(.pcor_pairwise_internal(x, y, use = "everything"))

  # Matrix for storing results
  cor_mat <- matrix(NA_real_, nrow = ncol(x), ncol = if (use_y) ncol(y) else ncol(x))
  if (!is.null(colnames(x)) || (use_y && !is.null(colnames(y))))
    dimnames(cor_mat) <- list(colnames(x), if (use_y) colnames(y) else colnames(x))

  # Use colnames to track column origin
  colnames(x) <- seq_len(ncol(x))
  if (use_y)
    colnames(y) <- seq_len(ncol(x))

  # Split by NA pattern
  x <- splitm(x, margin = 2, by_na_pattern = TRUE, min_size = min_ncol)
  if (use_y) {
    y <- splitm(y, margin = 2, by_na_pattern = TRUE, min_size = min_ncol)
  } else {
    y <- x
  }

  # Find which splits need to be compared for a full comparison
  comp_splits <- compute_comparisons(x, y = if (use_y) y else NULL, use = TRUE)

  single_comp <- nrow(comp_splits) == 1
  for (comp_idx in seq_len(nrow(comp_splits))) {
    i <- comp_splits$i[comp_idx]
    j <- comp_splits$j[comp_idx]
    use <- comp_splits$use[comp_idx]

    self_comp <- !use_y && i == j
    xx <- x[[i]]
    yy <- if (self_comp) NULL else y[[j]]

    # Self comparison on a diagonal only
    if (self_comp && ncol(xx) == 1) {
      xx_ind <- as.integer(colnames(xx))
      cor_mat[xx_ind, xx_ind] <- 1
      next()
    }

    res <- .pcor_pairwise_internal(xx, yy, use = use)

    if (single_comp) {
      dimnames(res) <- dimnames(cor_mat)
      cor_mat <- res
      next()
    }

    xx_ind <- as.integer(colnames(xx))
    yy_ind <- if (self_comp) xx_ind else as.integer(colnames(yy))

    cor_mat[xx_ind, yy_ind] <- res

    # Fill complement elements by transposing the indices not the matrices (faster)
    if (!(use_y || self_comp))
      cor_mat[as.matrix(expand.grid(xx_ind, yy_ind)[, 2:1])] <- res
  }

  cor_mat
}

.pcor_pairwise_internal <- function(x, y = NULL, use = "everything") {
  # Use stats::cor if there is no speed to be gained
  if (!use %in% c("everything", "all.obs", "complete.obs", "na.or.complete"))
    return(suppressWarnings(stats::cor(x, y, use = use, method = "pearson")))

  if (use == "all.obs") {
    if (anyNA(x))
      stop("Matrix 'x' contains missing values!")
    if (!is.null(y) && anyNA(y))
      stop("Matrix 'y' contains missing values!")

  } else if (use %in% c("complete.obs", "na.or.complete")) {
    if (is.null(y)) {
      not_na <- !is.na(x[, 1L])
    } else {
      not_na <- !(is.na(x[, 1L]) | is.na(y[, 1L]))
    }

    if (sum(not_na) < 2) {
      if (use == "complete.obs")
        stop("No values remaining after removing NAs")

      cor_mat <- matrix(NA_real_, nrow = ncol(x), ncol = if (!is.null(y)) ncol(y) else ncol(x))
      dimnames(cor_mat) <- list(colnames(x), if (is.null(y)) colnames(x) else colnames(y))
      attr(cor_mat, "n") <- 0L
      return(cor_mat)
    }

    if (!all(not_na)) {
      x <- x[not_na, , drop = FALSE]
      if (!is.null(y))
        y <- y[not_na, , drop = FALSE]
    }
  }

  t_mean_center <- function(mat) {
    mat <- t(mat)
    mat <- mat - rowMeans(mat)
    mat / sqrt(rowSums(mat^2))
  }

  # It is faster to do this check ourselves, although crossprod accepts NULL
  if (is.null(y)) {
    cor_mat <- tcrossprod(t_mean_center(x))
  } else {
    cor_mat <- tcrossprod(t_mean_center(x), t_mean_center(y))
  }
  attr(cor_mat, "n") <- nrow(x)
  cor_mat
}

# This function re-implements pcor_pairwise() to forego building the matrix for speed
##' @export
r2_cutoff <- function(x, y = NULL, cutoff, return_r2 = FALSE, use_colnames = FALSE, min_ncol = 20) {
  use_y <- !is.null(y)

  if (!use_colnames && is.list(x))
    stop("Lists are only supported if `use_colnames = TRUE`!\n",
         "Enabling this option is an advanced feature, you should know what you are doing.")

  # HACK by providing x & y as list and disabling use_colnames calling functions
  #      can supply matrices already split 'by_na_pattern', for a 10% speed gain
  pre_splitted <- use_colnames && is.list(x)

  if (pre_splitted) {
    if (!all(sapply(x, is.matrix)))
      stop("Not all elements of list 'x' are a matrix!")
    if (use_y) {
      if (!is.list(y))
        stop("Argument 'x' is provided as a list, but argument 'y' is not a list!")
      if (!all(sapply(y, is.matrix)))
        stop("Not all elements of list 'y' are a matrix!")
    }
  } else {
    stopifnot(is.matrix(x))
    if (use_y)
      stopifnot(is.matrix(y), nrow(x) == nrow(y))

    # Unclass so primitive functions are always used
    x <- unclass(x)
    if (use_y)
      y <- unclass(y)

    # If there are no NAs we can save ourselves a lot of hassle
    if (!anyNA(x) && !(use_y && anyNA(y)))
      return(.pcor_pairwise_internal(x, y, use = "everything"))

    # Ensure colnames are present to reconstruct the matrix
    if (!use_colnames || is.null(colnames(x)))
      colnames(x) <- seq_len(ncol(x))
    if (use_y && (!use_colnames || is.null(colnames(y))))
      colnames(y) <- seq_len(ncol(y))

    # Split by NA pattern
    x <- splitm(x, margin = 2, by_na_pattern = TRUE, min_size = min_ncol)
    if (use_y)
      y <- splitm(y, margin = 2, by_na_pattern = TRUE, min_size = min_ncol)
  }

  # Internal function to get the number of observations underlying an r2 score
  .get_n_observations <- function(r2, res, xx, yy = NULL) {
    n <- attr(r2, "n", exact = TRUE)
    if (!is.null(n))
      return(n)

    # NOTE If the 'n' attribute is null, 'xx' and/or 'yy' are impure 'below_min_col' matrices
    if (is.null(yy)) {
      if (anyNA(xx)) {
        n_na <- is.na(xx)
        n_na <- colSums(n_na[, res[, "row"], drop = FALSE] | n_na[, res[, "col"], drop = FALSE])
      } else {
        n_na <- 0L
      }
    } else {
      xx_has_na <- anyNA(xx)
      yy_has_na <- anyNA(yy)
      if (xx_has_na && yy_has_na) {
        n_na <- colSums(is.na(xx)[, res[,"row"], drop = FALSE] | is.na(yy)[, res[,"col"], drop = FALSE])
      } else if (xx_has_na) {
        n_na <- colSums(is.na(xx))[res[,"row"]]
      } else if (yy_has_na) {
        n_na <- colSums(is.na(yy))[res[,"col"]]
      } else {
        n_na <- 0L
      }
    }
    # Number of observations
    unname(nrow(xx) - as.integer(n_na))
  }

  # Find which splits need to be compared for a full comparison
  comp_splits <- compute_comparisons(x, y, use = TRUE)

  total <- lapply_rowwise(comp_splits, function(i, j, use) {
    self_comp <- !use_y && i == j
    xx <- unclass(x[[i]])
    yy <- if (!self_comp) {
      if (use_y) {
        unclass(y[[j]])
      } else {
        unclass(x[[j]])
      }
    }

    # Returns an [i x j] matrix (i as rows, j as cols)
    r2 <- unname(.pcor_pairwise_internal(xx, yy, use = use))^2

    res <- which(r2 > cutoff, arr.ind = TRUE)
    if (self_comp)
      res <- res[res[, 2] > res[, 1], , drop = FALSE]
    if (nrow(res) == 0L)
      return(NULL)

    if (return_r2) {
      n <- .get_n_observations(r2, res, xx, yy)
      r2 <- r2[res]
      res <- as.data.frame(res)
      res$r2 <- r2
      res$n <- n
    } else {
      res <- as.data.frame(res)
    }

    # Retrieve indexes to reflect position in overall matrix
    .get_idx <- function(a) as.integer(colnames(a))
    res$row <- .get_idx(xx)[res$row]
    res$col <- if (self_comp) .get_idx(xx)[res$col] else .get_idx(yy)[res$col]

    res
  })
  tibble::as_tibble(do.call(rbind, total))
}

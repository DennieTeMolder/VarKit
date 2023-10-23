# This function re-implements pcor_pairwise() to forego building the matrix for speed
##' @export
r2_cutoff <- function(x, y = NULL, cutoff, return_r2 = FALSE, check_colnames = TRUE, min_ncol = 20) {
  use_y <- !is.null(y)

  if (check_colnames && is.list(x))
    stop("Lists are only supported if `check_colnames = FALSE`!\n",
         "Enabling this option is an advanced feature, you should know what you are doing.")

  # HACK by providing x & y as list and disabling check_colnames calling functions
  #      can supply matrices already split 'by_na_pattern', for a 10% speed gain
  pre_splitted <- !check_colnames && is.list(x)

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

    # If there are no NAs we can save ourselves a lot of hassle
    if (!anyNA(x) && !(use_y && anyNA(y)))
      return(.pcor_pairwise_internal(x, y, use = "everything"))

    # Ensure colnames are present to reconstruct matrix
    if (is.null(colnames(x))) {
      colnames(x) <- seq_len(ncol(x))
    } else if (check_colnames) {
      stopifnot(!anyDuplicated(colnames(x)))
    }
    if (use_y) {
      if (is.null(colnames(y))) {
        colnames(y) <- seq_len(ncol(y))
      } else if (check_colnames) {
        stopifnot(!anyDuplicated(colnames(y)))
      }
    }

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
        n_na <- colSums(n_na[, res[,"row"], drop = FALSE] | n_na[, res[, "col"], drop = FALSE])
      } else {
        n_na <- 0
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
        n_na <- 0
      }
    }
    # Number of observations
    nrow(xx) - n_na
  }

  # Find which splits need to be compared for a full comparison
  comp_splits <- compute_comparisons(x, y, use = TRUE)

  total <- lapply_rowwise(comp_splits, function(i, j, use) {
    self_comp <- !use_y && i == j
    xx <- x[[i]]
    yy <- if (self_comp) NULL else if (use_y) y[[j]] else x[[j]]

    # Returns an [i x j] matrix (i as rows, j as cols)
    r2 <- .pcor_pairwise_internal(xx, yy, use = use)^2
    dimnames(r2) <- NULL

    res <- which(r2 > cutoff, arr.ind = TRUE)
    if (nrow(res) == 0)
      return(NULL)
    if (self_comp)
      res <- res[res[, 2] > res[, 1], , drop = FALSE]

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
    res$row <- colnames(xx)[res$row]
    res$col <- if (self_comp) colnames(xx)[res$col] else colnames(yy)[res$col]

    res
  })
  tibble::as_tibble(do.call(rbind, total))
}

.r2_cutoff_parallel <- function(mat_list, comps, cutoff, return_r2, cores) {
  if (cores > 1) {
    # TODO RcppParallel based implementation
    stop("Multi-core functionality is not implemented")
    restore_cores <- .set_cores(cores)
    on.exit(restore_cores())
  }

  purrr::map2(comps$i, comps$j, function(i, j) {
    x <- mat_list[[i]]
    y <- if (i == j) NULL else mat_list[[j]]
    r2_cutoff(x, y, cutoff = cutoff, return_r2 = return_r2, check_colnames = FALSE,)
  }, .progress = TRUE)
}

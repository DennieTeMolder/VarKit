# REVIEW adopt use of .dim()[margin]?
# REVIEW make each lapply its own function? We currently reference vars from the enclosing envs
##' @export
splitm <- function(x,
                   f = NULL,
                   size = NULL,
                   distances = NULL,
                   by_na_pattern = FALSE,
                   margin = 1,
                   min_size = 20,
                   keep_f = FALSE,
                   ...) {
  ## Required arguments ------
  .stopifnotsingle(margin, what = is.numeric)
  margin <- as.integer(margin)
  stopifnot(margin %in% 1:2)

  if (is.null(dim(x))) {
    if (!isTRUE(length(x) >= 0L))
      stop("Argument 'x' lacks either a dim() or length() property!")
    margin <- 0L
  } else if (length(dim(x)) != 2L) {
    # REVIEW: a 1d array raises an error, should we cast it to vector instead?
    stop("'x' should be a vector or two-dimensional object!")
  }

  .stopifnotsingle(min_size, what = is.numeric)
  .stopifnotsingle(by_na_pattern, what = "logical")
  .stopifnotsingle(keep_f, what = "logical")

  if (by_na_pattern) {
    if (margin == 0L) {
      warning("The arguments 'by_na_pattern' & 'min_size' are ignored for one-dimensional objects.")
      by_na_pattern <- FALSE
    }
  } else if (!missing(min_size) && min_size >= 1L) {
    warning("The argument 'min_size' is ignored when 'by_na_pattern = FALSE'.")
  }

  # Cast to integer after checking missing()
  min_size <- as.integer(min_size)

  ## Internal Functions ------
  .get_size <- switch(margin + 1L, length, nrow, ncol)

  .split_ind_formula <- function(x, f, margin, ...) {
    if (margin == 1L) {
      x <- x[, all.vars(f), drop = FALSE]
    } else if (margin == 2L) {
      x <- t(x[all.vars(f), , drop = FALSE])
    } else {
      stop("Unexpected")
    }

    x <- as.data.frame(x)
    x[[ncol(x) + 1L]] <- seq_len(nrow(x))

    x <- split.data.frame(x = x, f = f, ...)
    lapply(x, function(xx) xx[[ncol(xx)]])
  }

  .extract <- function(ind, data, margin) {
    if (identical(ind, 0L))
      return(data)

    # Return original object if all indices are selected (assuming no duplicates/NAs)
    if (length(ind) > 0L && ind[[1L]] == 1L) {
      total <- switch(margin + 1L, length, nrow, ncol)(data)
      if (total == length(ind))
        return(data)
    }

    switch(margin + 1L,
      data[ind],
      data[ind, , drop = FALSE],
      data[, ind, drop = FALSE]
    )
  }

  ## Optional arguments ------
  # Number of total values to split
  n <- .get_size(x)

  if (!is.null(distances))
    stopifnot(is.numeric(distances), length(distances) == n, !anyNA(distances))
  if (!is.null(size)) {
    .stopifnotsingle(size, what = is.numeric)
    if (is.null(distances))
      size <- as.integer(size)
  }

  ## Code ------
  # Split by f
  # Here we apply base::split() to indices to support splitting over margins
  # NOTE Using indices is 10x faster then using strings with unique() & ==
  if (is.null(f)) {
    # A value of 0L instructs .extract() to return the full object
    f_split_ind <- list(0L)
  } else if (inherits(f, "formula") && margin > 0L) {
    f_split_ind <- .split_ind_formula(x = x, f = f, margin = margin, ...)
    if (!keep_f) {
      if (margin == 1L) {
        x <- x[, !colnames(x) %in% all.vars(f), drop = FALSE]
      } else if (margin == 2L) {
        x <- x[!rownames(x) %in% all.vars(f), , drop = FALSE]
      } else {
        stop("Unexpected!")
      }
    }
  } else {
    f_split_ind <- split.default(seq_len(n), f = f, ...)
  }

  # Apply the splits and perform splitting by size if requested
  res <- lapply(f_split_ind, function(ind) {
    split <- .extract(ind, data = x, margin = margin)

    if (is.null(distances)) {
      dist <- seq_len(.get_size(split)) - 1L
    } else {
      dist <- .extract(ind, data = distances, margin = 0L)
    }

    # Split by size
    if (is.null(size) || length(dist) == 0L || size >= max(dist)) {
      size_split_ind <- list(0L)
    } else {
      size_split_ind <- unname(split.default(seq_along(dist), dist %/% size))
    }

    # Apply the splits and perform splitting by 'na pattern' if requested
    lapply(size_split_ind, function(ind2) {
      split2 <- .extract(ind2, data = split, margin = margin)

      if (!by_na_pattern)
        return(split2)

      # Split the current matrix further based on pattern of NA values if any
      if (!.any_na(split2))
        return(setNames(list(split2), nm = paste(rep(0L, nrow(split2)), collapse = "")))

      # Paste together missing values as a pattern of 0/1s
      is_na <- unname(is.na(split2))
      storage.mode(is_na) <- "integer"
      storage.mode(is_na) <- "character"
      is_na <- asplit(is_na, if (margin == 1L) 2L else 1L)
      is_na <- factor(do.call(paste0, is_na))

      # Group together "rare" na patterns for computational efficiency (based on MIN_SIZE)
      # The combined patterns end up in an impure matrix named "below_min_size"
      if (min_size > 1L && length(levels(is_na)) > 1L) {
        is_na <- forcats::fct_lump_min(is_na, min = min_size, other_level = "below_min_size")
        if (identical(levels(is_na), "below_min_size"))
          return(list(below_min_size = split2))
      }

      # Split such that each subset has the same pattern of NAs (except below_min_size)
      na_split_ind <- split.default(seq_along(is_na), is_na)
      lapply(na_split_ind, .extract, data = split2, margin = margin)
    })
  })

  # Un-nest list if specific arguments were omitted
  if (is.null(size))
    res <- lapply(res, function(xx) xx[[1L]])
  if (is.null(f))
    res <- res[[1L]]
  res
}

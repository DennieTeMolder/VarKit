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
  # Ensure margin is correct
  stopifnot(identical(1L, length(margin)), margin %in% 1:2)
  if (is.null(dim(x))) {
    if (!isTRUE(length(x) >= 0L))
      stop("Argument 'x' lacks either a dim() or length() property!")
    margin <- 0
  } else if (length(dim(x)) != 2L) {
    stop("'x' should be a one- or two- dimensional object")
  }

  ## Internal Functions ------
  .get_size <- switch(margin + 1, length, nrow, ncol)

  .split_ind_formula <- function(x, f, margin, ...) {
    stopifnot(margin %in% 1:2)

    if (margin == 2)
      x <- t(x)

    if (length(all.vars(f)) == 0) {
      x <- data.frame(.IDX = seq_len(nrow(x)))
    } else {
      x <- x[, unique(all.vars(f)), drop = FALSE]
      x <- cbind(x, .IDX = seq_len(nrow(x)))
    }

    x <- split.data.frame(x = x, f = f, ...)
    lapply(x, function(xx) as.integer(xx[, ".IDX", drop = TRUE]))
  }

  .extract <- function(ind, data, margin) {
    if (identical(ind, 0L))
      return(data)
    if (ind[1] == 1L) {
      total <- switch(margin + 1, length, nrow, ncol)(data)
      if (total == length(ind))
        return(data)
    }

    split <- switch(margin + 1,
      data[ind],
      data[ind, , drop = FALSE],
      data[, ind, drop = FALSE]
    )
    class(split) <- class(data)
    split
  }

  ## Argument checks ------
  n <- .get_size(x)

  # Required
  stopifnot(is.numeric(min_size), length(min_size) == 1L)
  if (!missing(min_size) && !by_na_pattern)
    warning("The argument 'min_size' is ignored when 'by_na_pattern = FALSE'.")
  if (by_na_pattern && margin == 0) {
    warning("The arguments 'by_na_pattern' & 'min_size' are ignored for one-dimensional objects.")
    by_na_pattern <- FALSE
  }

  # Optional
  if (!is.null(distances))
    stopifnot(is.numeric(distances), length(distances) == n)
  if (!is.null(size))
    stopifnot(is.numeric(size), length(size) == 1)

  ## Code ------
  # Split by f
  # Implement base::split() with support for margins
  if (is.null(f)) {
    f_split_ind <- list(0L) # Instruct .extract to return the full object
  } else if (inherits(f, "formula") && margin != 0) {
    f_split_ind <- .split_ind_formula(x = x, f = f, margin = margin, ...)
    if (!keep_f) {
      if (margin == 1) {
        x <- x[, !colnames(x) %in% all.vars(f), drop = FALSE]
      } else {
        x <- x[!rownames(x) %in% all.vars(f), , drop = FALSE]
      }
    }
  } else {
    f_split_ind <- split(seq_len(n), f = f, ...)
  }

  res <- lapply(f_split_ind, function(ind) {
    split <- .extract(ind, x, margin)
    dist <- if (is.null(distances)) NULL else .extract(ind, distances, margin = 0)

    # Split by size
    max_size <- if (is.null(dist)) .get_size(split) else diff(range(dist))

    if (is.null(size) || size >= max_size) {
      size_split_ind <- list(0L)
    } else {
      if (is.null(dist)) {
        size_split_ind <- rep(seq(0, max_size / size), each = size, length.out = max_size)
      } else {
        size_split_ind <- min(dist) + seq(size, max_size + 1, by = size)
        size_split_ind <- findInterval(dist, size_split_ind)
      }
      size_split_ind <- split(seq_along(size_split_ind), size_split_ind)
      names(size_split_ind) <- NULL
    }

    # NOTE Using indices is 10x faster then using unique() & ==
    lapply(size_split_ind, function(ind2) {
      split2 <- .extract(ind2, split, margin)

      if (!by_na_pattern)
        return(split2)

      min_size_current <- min(min_size, if (is.null(dist)) max_size else .get_size(split2))

      # Split the current matrix further based on pattern of NA values if any
      if (!anyNA(split2))
        return(setNames(list(split2), paste(rep(0L, nrow(split2)), collapse = "")))

      # Paste together missing values as a pattern of 0/1s
      is_na <- is.na(split2)
      dimnames(is_na) <- NULL
      storage.mode(is_na) <- "integer"
      storage.mode(is_na) <- "character"
      is_na <- asplit(is_na, if (margin == 1) 2 else 1)
      is_na <- factor(do.call(paste0, is_na))

      # Group together "rare" na patterns for computational efficiency
      # The combined patterns end up in an impure matrix named "below_min_size"
      if (min_size_current > 1 && length(levels(is_na)) > 1) {
        is_na <- forcats::fct_lump_min(is_na, min = min_size_current, other_level = "below_min_size")
        if (length(levels(is_na)) == 1)
          return(list(below_min_size = split2))
      }

      # Split such that each subset has the same pattern of NAs (except below_min_size)
      lapply(split(seq_along(is_na), is_na), .extract, split2, margin)
    })
  })

  # Un-nest list if specific arguments were omitted
  if (is.null(size))
    res <- lapply(res, function(x) x[[1]])
  if (is.null(f))
    res <- res[[1]]
  res
}

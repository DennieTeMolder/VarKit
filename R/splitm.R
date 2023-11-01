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
  stopifnot(is.numeric(margin), length(margin) == 1L)
  margin <- as.integer(margin)
  stopifnot(margin %in% 1:2)
  if (is.null(dim(x))) {
    if (!isTRUE(length(x) >= 0L))
      stop("Argument 'x' lacks either a dim() or length() property!")
    margin <- 0L
  } else if (length(dim(x)) != 2L) {
    stop("'x' should be a one- or two- dimensional object")
  }

  ## Internal Functions ------
  .get_size <- switch(margin + 1L, length, nrow, ncol)

  .split_ind_formula <- function(x, f, margin, ...) {
    stopifnot(margin %in% 1:2)

    if (margin == 2L)
      x <- t(x)

    if (length(all.vars(f)) == 0L) {
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

  ## Argument checks ------
  n <- .get_size(x)

  # Required
  stopifnot(is.numeric(min_size), length(min_size) == 1L)
  if (!missing(min_size) && !by_na_pattern)
    warning("The argument 'min_size' is ignored when 'by_na_pattern = FALSE'.")
  if (by_na_pattern && margin == 0L) {
    warning("The arguments 'by_na_pattern' & 'min_size' are ignored for one-dimensional objects.")
    by_na_pattern <- FALSE
  }
  min_size <- as.integer(min_size)

  # Optional
  if (!is.null(distances))
    stopifnot(is.numeric(distances), length(distances) == n)
  if (!is.null(size)) {
    stopifnot(is.numeric(size), length(size) == 1L)
    if (is.null(distances))
      size <- as.integer(size)
  }

  ## Code ------
  # Split by f
  # Implement base::split() with support for margins
  if (is.null(f)) {
    f_split_ind <- list(0L) # Instruct .extract to return the full object
  } else if (inherits(f, "formula") && margin > 0L) {
    f_split_ind <- .split_ind_formula(x = x, f = f, margin = margin, ...)
    if (!keep_f) {
      if (margin == 1L) {
        x <- x[, !colnames(x) %in% all.vars(f), drop = FALSE]
      } else {
        x <- x[!rownames(x) %in% all.vars(f), , drop = FALSE]
      }
    }
  } else {
    f_split_ind <- split(seq_len(n), f = f, ...)
  }

  # NOTE Using indices is 10x faster then using unique() & ==
  res <- lapply(f_split_ind, function(ind) {
    split <- .extract(ind, data = x, margin = margin)
    if (is.null(distances)) {
      dist <- seq_len(.get_size(split)) - 1L
    } else {
      dist <- .extract(ind, data = distances, margin = 0L)
    }

    # Split by size
    if (is.null(size) || size >= max(dist)) {
      size_split_ind <- list(0L)
    } else {
      size_split_ind <- unname(split(seq_along(dist), dist %/% size))
    }

    lapply(size_split_ind, function(ind2) {
      split2 <- .extract(ind2, data = split, margin = margin)

      if (!by_na_pattern)
        return(split2)

      # Split the current matrix further based on pattern of NA values if any
      if (!anyNA(split2))
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
      lapply(split(seq_along(is_na), is_na), .extract, data = split2, margin = margin)
    })
  })

  # Un-nest list if specific arguments were omitted
  if (is.null(size))
    res <- lapply(res, function(x) x[[1L]])
  if (is.null(f))
    res <- res[[1L]]
  res
}

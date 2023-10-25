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
  stopifnot(margin %in% 1:2)
  if (is.null(dim(x))) {
    if (!isTRUE(length(x) >= 0))
      stop("Argument 'x' lacks either a dim() or length() property!")
    margin <- 0
  }

  ## Internal Functions ------
  .get_size <- switch(margin + 1, length, nrow, ncol)

  # Based on base::.formula2varlist(), but supports margin
  .apply_formula <- function(f, data, margin) {
    if (length(f) == 3)
      warning("Unexpected LHS in 'f' has been combined with RHS.")

    vars <- all.vars(f)
    if (length(vars) == 0)
      stop("Could not extract named variables from 'f'!")

    fun_name <- switch(margin + 1, "names", "colnames", "rownames")
    d_names <- do.call(fun_name, list(data), envir = baseenv())

    if (is.null(d_names))
      stop("Cannot apply formula, ", fun_name, "(x) is NULL!")
    if (!all(vars %in% d_names))
      stop("Formula 'f' contains terms not found in ", fun_name, "(x)!")

    names(vars) <- vars
    lapply(vars, function(var) {
      unlist(switch(margin + 1, data[[var]], data[, var], data[var, ]), use.names = TRUE)
    })
  }

  .extract <- function(ind, data, margin) {
    if (identical(ind, 0))
      return(data)
    if (ind[1] == 1) {
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
  if (margin != 0)
    stopifnot(length(dim(x)) == 2)
  stopifnot(is.numeric(min_size), length(min_size) == 1)
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
  # Implement base::split() with support for margins
  if (is.null(f)) {
    f_split_ind <- list(0) # Instruct .extract to return the full object
  } else {
    if (inherits(f, "formula")) {
      f <- .apply_formula(f, x, margin)
      if (!keep_f) {
        x <- switch(margin + 1,
          x <- x[!names(x) %in% names(f)],
          x <- x[, !colnames(x) %in% names(f), drop = FALSE],
          x <- x[!rownames(x) %in% names(f), , drop = FALSE]
        )
      }
    }
    f_split_ind <- split(seq_len(n), f = f, ...)
  }

  # Split by f
  res <- lapply(f_split_ind, function(ind) {
    split <- .extract(ind, x, margin)
    dist <- if (is.null(distances)) NULL else .extract(ind, distances, margin = 0)

    max_size <- if (is.null(dist)) .get_size(split) else diff(range(dist))

    if (is.null(size) || size >= max_size) {
      size_split_ind <- list(0)
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

    # Split by size
    # NOTE Using indices is 10x faster then using unique() & ==
    lapply(size_split_ind, function(ind2) {
      split2 <- .extract(ind2, split, margin)

      if (!by_na_pattern)
        return(split2)

      min_size_current <- min(min_size, if (is.null(dist)) max_size else .get_size(split2))

      # Split the current matrix further based on pattern of NA values if any
      if (!anyNA(split2))
        return(setNames(list(split2), paste(rep(0, nrow(split2)), collapse = "")))

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

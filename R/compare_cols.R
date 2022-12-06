# FUN is a predicate that takes vector A and matrix B (length(a) == nrow(b)) and
# returns a vector with TRUE for every column in B that it considers 'identical' to A
.compare_cols <- function(x,
                          FUN,
                          pos = NULL,
                          window = NULL,
                          llist = NULL,
                          ...) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  n <- ncol(x)

  # Missing arguments
  if (is.null(window))
    window <- .window_default(use_bp = !is.null(pos))
  if (is.null(llist))
    llist <- link_list(seq_len(n))

  # Check arguments
  stopifnot(is.function(FUN))
  if (!is.null(pos)) {
    stopifnot(is.vector(pos) && is.numeric(pos))
    if (length(pos) != n)
      stop("The length of 'pos' should match with ncol(x)!")
  }
  if (!is.null(window))
    .stopifnotsingle(window, class = "numeric")
  stopifnot(is_link_list(llist))
  if (length(llist) != n)
    stop("The length of 'link_list' does not match with 'x'!")

  # Disable window if it is larger then x
  if (!.window_disabled(window)) {
    if (is.null(pos)) {
      if (window / 2 >= n)
        window <- -1
    } else {
      if (window / 2 >= max(pos) - min(pos))
        window <- -1
    }
  }

  # Remove names for speed
  colnames(x) <- rownames(x) <- NULL

  # Columns that are all NA are not worth checking
  n_na <- colSums(is.na(x))
  check <- n_na < nrow(x)

  # Link columns with all NAs
  if (sum(!check) > 1) {
    all_na <- which(!check)
    llist <- link_entries(llist, idx = all_na[1], links = all_na[-1])
  }

  # Remove last as it has been in every possible comparison
  n_comp <- sum(check) - 1

  # No comparisons to make
  if (n_comp < 1)
    return(llist)

  # Work from least to most NAs to prevent sub-optimal linking
  check_order <- utils::head(order(n_na, decreasing = FALSE), n = n_comp)

  for (i in check_order) {
    if (!check[i])
      next()

    check[i] <- FALSE
    check_idx <- .which_in_window(i, check, window, pos, n)

    if (length(check_idx) < 1)
      next()

    # Check which cols in B can be considered identical to A using predicate FUN
    identical <- do.call(FUN, list(a = x[, i], b = x[, check_idx, drop = FALSE], ...))
    if (sum(identical) < 1)
      next()

    links <- check_idx[identical]
    llist <- link_entries(llist, idx = i, links = links)
    check[links] <- FALSE
  }
  redistribute(llist, data = x, keep = "least_na", tiebreaker = "middle")
}

.window_default <- function(use_bp = FALSE) {
  if (use_bp) {
    window <- 1e5
    message("Setting window size to ", window," bp.")
    return(window)
  }
  window <- 1000
  message("Setting window size to ", window," variants.")
  window
}

.window_disabled <- function(window) {
  # Sizes smaller then 1 disable the window
  window < 1
}

.which_in_window <- function(i, check, window, pos, n) {
  if (.window_disabled(window))
    return(which(check))

  window <- ceiling(window / 2)

  # Window using bp distance
  if (!is.null(pos))
    return(which(check & pos > pos[i] - window & pos <= pos[i] + window))

  # Window using # variants
  min <- max(c(i - window, 1))
  max <- min(c(i + window, n))
  range <- min:max
  range[check[range]]
}

# FUN is a predicate that takes vector A and matrix B (length(a) == nrow(b)) and
# returns a vector with TRUE for every column in B that it considers 'identical' to A
.compare_cols <- function(x,
                          FUN,
                          pos = NULL,
                          window = NULL,
                          llist = NULL,
                          ...) {
  # Missing arguments
  if (is.null(window))
    window <- .window_default(use_bp = !is.null(pos))
  if (is.null(llist))
    llist <- link_list(1:ncol(x))

  n <- ncol(x)

  # Check arguments
  stopifnot(is.matrix(x) || is.data.frame(x))
  stopifnot(is.function(FUN))
  if (!is.null(pos)) {
    stopifnot(is.vector(pos))
    stopifnot(is.numeric(pos))
    if (length(pos) != n)
      stop("The length of 'pos' should match with ncol(x)!")
  }
  if (!is.null(window))
    stopifnot(is.numeric(window))
  stopifnot(is_link_list(llist))
  if (length(llist) != n)
    stop("The length of 'link_list' does not match with 'x'!")

  if (ncol(x) == 1)
    return(llist)

  # Remove names for speed
  colnames(x) <- rownames(x) <- NULL

  # Work from least to most NAs to prevent sub-optimal linking
  check_order <- order(colSums(is.na(x)))

  check <- rep(TRUE, n)
  for (i in check_order) {
    if (!check[i])
      next()

    check[i] <- FALSE
    check_idx <- .which_in_window(i, check, window, pos, n)

    # Check which cols in B can be considered identical to A using predicate FUN
    identical <- do.call(FUN, list(a = x[, i], b = x[, check_idx, drop = FALSE], ...))
    if (sum(identical) == 0)
      next()

    links <- check_idx[identical]
    llist <- link_entries(llist, idx = i, links = links)
    check[links] <- FALSE
  }
  redistribute(llist, data = x, keep = "least_na", tiebreaker = "middle")
}

.window_default <- function(use_bp = FALSE) {
  if (use_bp) {
    window <- 5e4
    message("Setting window size to ", window," bp.")
    return(window)
  }
  window <- 100
  message("Setting window size to ", window," variants.")
  window
}

.which_in_window <- function(i, check, window, pos, n) {
  # Sizes smaller then 1 disable the window
  if (window < 1)
    return(which(check))

  window <- ceiling(window / 2)

  # Window using bp distance
  if (!is.null(pos))
    return(which(check & pos > pos[i] - window & pos <= pos[i] + window))

  # Window using # variants
  min <- ifelse(i > window, i - window, 1)
  max <- ifelse(i + window < n, i + window, n)
  which(check)[min:max]
}

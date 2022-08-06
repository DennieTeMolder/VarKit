##' @export
find_identical_cols <- function(x, tolerance = .01, llist = NULL) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  n <- ncol(x)

  # Check arguments
  stopifnot(is.numeric(tolerance) && tolerance >= 0)
  if (!is.null(llist)) {
    stopifnot(is_link_list(llist))
    if (length(llist) != ncol(x))
      stop("The length of 'link_list' does not match with 'x'!")
  } else {
    llist <- link_list(1:n)
  }

  # Remove names for speed
  colnames(x) <- rownames(x) <- NULL

  # Keep track of indexes that can be skipped
  skip <- rep(FALSE, n)
  # Skip last column as it has already been compared to everything
  for (i in seq(1, n - 1)) {
    # Continue to next if current column was already removed
    if (skip[i])
      next()

    # Only check columns after as prior have been compared already
    check <- seq(i + 1, n)
    check <- check[!skip[check]]
    if (length(check) == 0)
      next()

    # Compute equality using matrix arithmetic for speed
    diff <- x[, check, drop = FALSE] - x[, i]
    sums <- colMeans(abs(diff), na.rm = TRUE)
    identical <- check[sums <= tolerance]

    identical <- identical[!is.na(identical)]
    if (length(identical) == 0)
      next()

    # From all columns with the least number of NAs, select the middle column to be retained
    combined <- c(i, identical)
    keep <- .which_least_na_middle(combined, data = x)
    llist <- link_entries(llist, combined[keep], combined[-keep])
    skip[combined[-keep]] <- TRUE
  }
  llist
}

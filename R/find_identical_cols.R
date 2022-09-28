##' @export
find_identical_cols <- function(x,
                                tolerance = .01,
                                method = "fast",
                                llist = NULL) {
  # Check arguments
  stopifnot(is.matrix(x) || is.data.frame(x))
  stopifnot(is.numeric(tolerance) && tolerance >= 0)
  stopifnot(method %in% c("fast", "recursive"))
  if (!is.null(llist)) {
    stopifnot(is_link_list(llist))
    if (length(llist) != ncol(x))
      stop("The length of 'link_list' does not match with 'x'!")
  } else {
    llist <- link_list(1:ncol(x))
  }

  # Remove names for speed
  colnames(x) <- rownames(x) <- NULL

  if (method == "fast")
    return(.find_identical_cols_fast(x, tolerance, llist))
  if (method == "recursive")
    return(.find_identical_cols_recursive(x, tolerance, llist))
  stop("Unexpected!")
}

.find_identical_cols_fast <- function(x, tolerance, llist) {
  n <- ncol(x)
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

.find_identical_cols_recursive <- function(x, tolerance, llist) {
  n <- ncol(x)
  identical_cols <- vector(mode = "list", length = n)
  # Skip last column as it has already been compared to everything
  for (i in seq(1, n - 1)) {
    # Only check columns after as prior have been compared already
    check <- seq(i + 1, n)

    # Compute equality using matrix arithmetic for speed
    diff <- x[, check, drop = FALSE] - x[, i]
    sums <- colMeans(abs(diff), na.rm = TRUE)
    identical <- check[sums <= tolerance]

    identical <- identical[!is.na(identical)]
    if (length(identical) == 0)
      next()

    # From all columns with the least number of NAs, select the middle column to be retained
    identical_cols[[i]] <- c(i, identical)
  }

  resolve_linked_cols(identical_cols, data = x, llist = llist)
}

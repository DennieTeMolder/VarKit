##' @export
find_identical_cols <- function(x,
                                tolerance = .01,
                                llist = NULL) {
  # Missing arguments
  if (is.null(llist))
    llist <- link_list(seq_len(ncol(x)))

  # Check arguments
  stopifnot(is.matrix(x) || is.data.frame(x))
  stopifnot(is.numeric(tolerance) && tolerance >= 0)
  stopifnot(is_link_list(llist))
  if (length(llist) != ncol(x))
    stop("The length of 'link_list' does not match with 'x'!")

  if (ncol(x) == 1)
    return(llist)

  # Remove names for speed
  colnames(x) <- rownames(x) <- NULL

  .find_identical_cols_fast(x, tolerance, llist)
}

.find_identical_cols_fast <- function(x, tolerance, llist) {
  # Work from least to most NAs to prevent sub-optimal linking
  check_order <- order(colSums(is.na(x)))
  check <- rep(TRUE, ncol(x))
  for (i in check_order) {
    if (!check[i])
      next()

    # Check across the whole matrix, only checking after 'i' will result in
    # comparisons with more NAs in 'i'
    check[i] <- FALSE
    check_idx <- which(check)

    # Compute equality using matrix arithmetic for speed
    diff <- x[, check_idx, drop = FALSE] - x[, i]
    sums <- colMeans(abs(diff), na.rm = TRUE)
    identical <- check_idx[sums <= tolerance & !is.na(sums)]

    if (length(identical) == 0)
      next()

    llist <- link_entries(llist, idx = i, links = identical)
    check[identical] <- FALSE
  }
  redistribute(llist, data = x, keep = "middle")
}

# REVIEW this function should yield identical results as above, but slower,
# provided comparisons are done across the entire matrix/df.
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

    identical_cols[[i]] <- c(i, identical)
  }

  resolve_linked_cols(identical_cols, data = x, llist = llist)
}

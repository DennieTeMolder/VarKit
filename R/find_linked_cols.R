##' @export
find_linked_cols <- function(x,
                             pos = NULL,
                             cutoff = .9,
                             window = NULL,
                             method = "recursive",
                             llist = NULL) {
  # Check arguments
  stopifnot(is.matrix(x) || is.data.frame(x))
  if (!is.null(pos)) {
    stopifnot(is.vector(pos))
    stopifnot(is.numeric(pos))
    if (length(pos) != ncol(x))
      stop("The length of 'pos' should match with ncol(x)!")
  }
  stopifnot(is.numeric(cutoff))
  if (!is.null(window))
    stopifnot(is.numeric(window))
  stopifnot(method %in% c("fast", "recursive"))
  if (!is.null(llist)) {
    stopifnot(is_link_list(llist))
    if (length(llist) != ncol(x))
      stop("The length of 'link_list' does not match with 'x'!")
  }

  # Missing arguments
  if (is.null(window))
    window <- .window_default(use_bp = !is.null(pos))
  if (is.null(llist))
    llist <- link_list(1:ncol(x))

  # Remove names for speed
  colnames(x) <- rownames(x) <- NULL

  if (method == "fast")
    return(.find_linked_cols_fast(x, pos, cutoff, window, llist))
  if (method == "recursive")
    return(.find_linked_cols_recursive(x, pos, cutoff, window, llist))
  stop("Unexpected!")
}

.find_linked_cols_fast <- function(x, pos, cutoff, window, llist) {
  n <- ncol(x)
  skip <- rep(FALSE, n)
  # Skip last column as it has already been compared to everything
  for (i in seq(1, n - 1)) {
    # Continue to next if current column was already removed
    if (skip[i])
      next()

    # Find columns right of 'i' and within window
    if (window < 1) {
      check <- seq(i + 1, n)
    } else if (is.null(pos)) {
      check <- 1:window + i
      check <- check[check <= n]
    } else {
      check <- which(pos > pos[i] & pos <= pos[i] + window)
    }

    check <- check[!skip[check]]
    if (length(check) == 0)
      next()

    # Find cols above linkage cutoff
    r2 <- .cor_pair_silent(x[, i], x[, check, drop = FALSE])^2
    above <- check[r2 >= cutoff]

    above <- above[!is.na(above)]
    if (length(above) == 0)
      next()

    # From all columns with the least number of NAs, select the middle column to be retained
    combined <- c(i, above)
    keep <- .which_least_na_middle(combined, data = x)
    llist <- link_entries(llist, combined[keep], combined[-keep])
    skip[combined[-keep]] <- TRUE
  }
  llist
}

.find_linked_cols_recursive <- function(x, pos, cutoff, window, llist) {
  n <- ncol(x)
  linked_cols <- vector(mode = "list", length = n)
  for (i in seq(1, n - 1)) {
    # Find columns right of 'i' and within window
    if (window < 1) {
      check <- seq(i + 1, n)
    } else if (is.null(pos)) {
      check <- 1:window + i
      check <- check[check <= n]
    } else {
      check <- which(pos > pos[i] & pos <= pos[i] + window)
    }

    if (length(check) == 0)
      next()

    # Find cols above linkage cutoff
    r2 <- .cor_pair_silent(x[, i], x[, check, drop = FALSE])^2
    above <- check[r2 >= cutoff]

    above <- above[!is.na(above)]
    if (length(above) == 0)
      next()

    # Each col contains its own index, we use this during the resolve phase
    linked_cols[[i]] <- c(i, above)
  }

  resolve_linked_cols(linked_cols, data = x, llist = llist)
}

.window_default <- function(use_bp = FALSE) {
  if (use_bp) {
    window <- 5e4
    message("Setting window size to ", window," bp.")
    return(window)
  }
  window <- 100
  message("Setting window size to ", window," sites.")
  window
}

.cor_pair_silent <- function(x, y = NULL) {
  suppressWarnings(stats::cor(x, y, use = "pairwise.complete.obs"))
}


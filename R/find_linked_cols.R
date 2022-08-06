##' @export
find_linked_cols <- function(x,
                             pos = NULL,
                             cutoff = .9,
                             window = NULL,
                             llist = NULL) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  n <- ncol(x)

  # Checks
  if (!is.null(pos)) {
    stopifnot(is.vector(pos))
    stopifnot(is.numeric(pos))
    if (length(pos) != n)
      stop("The length of 'pos' should match with ncol(x)!")
  }
  stopifnot(is.numeric(cutoff))
  if (!is.null(window)) {
    stopifnot(is.numeric(window))
  } else {
    if (is.null(pos)) {
      window <- 100
      message("Setting window size to ", window," sites.")
    } else {
      window <- 5e4
      message("Setting window size to ", window," bp.")
    }
  }
  if (!is.null(llist)) {
    stopifnot(is_link_list(llist))
    if (length(llist) != ncol(x))
      stop("The length of 'link_list' does not match with 'x'!")
  } else {
    llist <- link_list(1:n)
  }

  # Keep track of indexes that can be skipped
  skip <- rep(FALSE, n)
  for (i in seq(1, n - 1)) {
    if (skip[i])
      next()

    # Find columns right of sample and within window
    if (is.null(pos)) {
      check <- 1:window + i
      check <- check[check <= n]
    } else {
      check <- which(pos > pos[i] & pos <= pos[i] + window)
    }

    check <- check[!skip[check]]
    if (length(check) == 0)
      next()

    # Find cols above linkage cutoff
    r2 <- .cor_pair_silent(x[, i], x[, check , drop = FALSE])^2
    above <- check[r2 >= cutoff & !is.na(r2)]

    if (length(above) == 0)
      next()

    combined <- c(i, above)
    keep <- .which_least_na_middle(combined, data = x)
    llist <- link_entries(llist, combined[keep], combined[-keep])
    skip[combined[-keep]] <- TRUE
  }
  llist
}

.cor_pair_silent <- function(x, y = NULL) {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
}

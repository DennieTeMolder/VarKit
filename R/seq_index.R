##' @export
seq_index <- function(x, n_max = NULL) {
  UseMethod("seq_index", x)
}

##' @export
seq_index.varkit_index <- function(x, n_max = NULL) {
  if (nrow(x) < 1L)
    stop(substitute(x), " is zero length!")

  if (is.null(n_max))
    n_max <- read_vcf_args(x)$n_max
  if (!is.null(n_max))
    .stopifnotsingle(n_max, what = is.numeric)

  # Ignore negative N_MAX
  if (!isTRUE(n_max >= 1L))
    n_max <- NULL

  pos <- .seek_next(x)
  if (pos$n_lines < 1L) # Pointer outside of x
    return(integer())

  # Add the row position index and select applicable data
  x$idx <- seq_len(nrow(x))

  # Subset from current position to end of index, remove empty lines
  x <- x[seq(as.integer(rownames(pos)), nrow(x)), ]
  x <- x[x$n_lines >= 1L, c("id", "n_lines", "idx")]

  # Simple case if N_MAX does not apply
  if (is.null(n_max) || !any(x$n_lines > n_max))
    return(setNames(x$idx, x$id))

  # If N_MAX does apply we repeat the index
  do.call(c, mapply(x$idx, x$id, ceiling(x$n_lines / n_max), FUN = function(idx, id, n) {
    rep(setNames(idx, id), n)
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE))
}

##' @export
mtrx <- function(x, dim_names = NULL) {
  x <- as.matrix(x)
  if (!is.null(dim_names))
    names(dimnames(x)) <- dim_names

  class(x) <- c("mtrx", setdiff(class(x), "mtrx"))
  x
}

##' @export
is_mtrx <- function(x) {
  inherits(x, "mtrx")
}

##' @export
print.mtrx <- function(x, ..., n = NULL, n_col = NULL) {
  rlang::check_dots_used()

  if (is.null(n))
    n <- getOption("print_min", 10L)
  n <- min(n, nrow(x))

  row_trunc <- isTRUE(n < nrow(x))
  row_idx <- if (row_trunc) seq_len(n) else rlang::missing_arg()

  if (is.null(n_col))
    n_col <- .max_print_col(x, row_idx = row_idx)
  n_col <- min(n_col, ncol(x))

  col_trunc <- isTRUE(n_col < ncol(x))
  col_idx <- if (col_trunc) seq_len(n_col) else rlang::missing_arg()

  if (is.null(names(dimnames(x)))) {
    row_name <- col_name <- NA
  } else {
    row_name <- names(dimnames(x))[1L]
    col_name <- names(dimnames(x))[2L]
  }

  row_name <- if (is.na(row_name)) " rows" else paste0(" ", row_name)
  col_name <- if (is.na(col_name)) " columns" else paste0(" ", col_name)

  cat(sprintf(
    "# A matrix<%s>: %s%s x %s%s; %s%% is NA/NaN\n",
    typeof(x), .frmt_big(nrow(x)), row_name, .frmt_big(ncol(x)), col_name,
    round(mean(is.na(x)) * 100, digits = 2)
  ))

  # Remove the dimnames names te prevent them from printing
  xp <- x
  names(dimnames(xp)) <- NULL
  print.default(unclass(xp[row_idx, col_idx, drop = FALSE]))

  if (row_trunc && col_trunc) {
    .msg_info(.frmt_big(nrow(x) - n), " more", row_name, " and ", .frmt_big(ncol(x) - n_col), " more", col_name)
    .msg_info("Use `print(n = ..., n_col = ...)` to see more", row_name, " and", col_name, " respectively.")
  } else if (row_trunc) {
    .msg_info(.frmt_big(nrow(x) - n), " more", row_name)
    .msg_info("Use `print(n_col = ...)` to see more", row_name)
  } else if (col_trunc) {
    .msg_info(.frmt_big(ncol(x) - n_col), " more", col_name)
    .msg_info("Use `print(n = ...)` to see more", col_name)
  }

  invisible(x)
}

# Contrary to base R we set 'drop = F' mirroring tibble:::`[.tbl_df`
##' @export
`[.mtrx` <- function(x, i, j, drop = FALSE, ...) {
  class(x) <- setdiff(class(x), "mtrx")
  res <- x[i, j, drop = drop]
  if (is.matrix(res))
    class(res) <- c("mtrx", class(res))
  res
}

.max_print_col <- function(mat, row_idx = NULL, width = getOption("width")) {
  stopifnot(is.matrix(mat))
  if (!missing(row_idx) && is.null(row_idx))
    row_idx <- rlang::missing_arg()
  width <- as.integer(width)

  .max_nchar <- function(x) {
    max(nchar(format(x, trim = TRUE)))
  }

  # Reduce size of matrix to the maximum possible number of columns
  n_max <- max(1L, as.integer(floor(width / 2L)) - 1L)
  if (ncol(mat) > n_max) {
    mat <- mat[row_idx, seq_len(n_max), drop = FALSE]
  } else {
    mat <- mat[row_idx, , drop = FALSE]
  }

  if (is.null(rownames(mat))) {
    rname_len <- nchar(nrow(mat)) + 3L
  } else {
    rname_len <- .max_nchar(rownames(mat))
  }

  if (is.null(colnames(mat))) {
    # Add 3 for the [,] and 1 for the space before the column
    cname_len <- nchar(seq_len(ncol(mat))) + 4L
  } else {
    # Add 2 here for the potential `` around var names + 1 for the space
    cname_len <- nchar(colnames(mat)) + 3L
  }

  # Add 1 for space between columns and 2 for the quotes around character values
  cval_len <- apply(mat, 2, .max_nchar) + 1L
  if (is.character(mat))
    cval_len <- cval_len + 2L

  # Check if col name or col value is widest, add row name width to first element
  col_widths <- pmax.int(cname_len, cval_len)
  col_widths[1L] <- col_widths[1L] + rname_len

  # Return the index of the last column that fits within width
  which_fit <- which(cumsum(col_widths) < width)
  max(which_fit, 1L)
}

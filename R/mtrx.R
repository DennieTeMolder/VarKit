##' @export
mtrx <- function(data, nrow, ncol, byrow = FALSE, dimnames = NULL) {
  if (is.character(dimnames) && length(dimnames) <= 2L)
    dimnames <- setNames(vector("list", 2L), dimnames)

  # Passing rlang::missing_arg() raises an error
  res <- if (missing(data) || is.null(data)) {
    matrix(logical(), dimnames = dimnames)[NULL, NULL]
  } else if (missing(nrow) && missing(ncol)) {
    matrix(data, byrow = byrow, dimnames = dimnames)
  } else if (missing(ncol)) {
    matrix(data, nrow, byrow = byrow, dimnames = dimnames)
  } else {
    matrix(data, nrow, ncol, byrow = byrow, dimnames = dimnames)
  }

  class(res) <- c("mtrx", class(res))
  res
}

##' @export
is_mtrx <- function(x) {
  inherits(x, "mtrx")
}

##' @export
as_mtrx <- function(x, dimnames = NULL, ...) {
  if (is.null(x))
    return(mtrx(x, dimnames = dimnames))

  if (!is_mtrx(x)) {
    x <- as.matrix(x, ...)
    class(x) <- c("mtrx", class(x))
  }

  if (!is.null(dimnames)) {
    if (is.character(dimnames) && length(dimnames) <= 2L)
      dimnames <- setNames(list(rownames(x), colnames(x)), dimnames)
    dimnames(x) <- dimnames
  }

  x
}

##' @export
print.mtrx <- function(x, ..., n = NULL, n_col = NULL, advice = getOption("mtrx.advice")) {
  advice <- !isFALSE(advice)
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

  # Ensure max.print doesn't interfere
  rlang::local_options(max.print = .Machine$integer.max)

  ndn <- .namesdimnames(x)
  row_name <- paste0(" ", ndn[1L])
  col_name <- paste0(" ", ndn[2L])

  # Print header
  .catn(.style_subtle(
    sprintf(
      "# A matrix<%s>: %s%s %s %s%s",
      typeof(x), .frmt_big(nrow(x)), row_name,
      cli::symbol$times, .frmt_big(ncol(x)), col_name
    ),
    if (.any_na(x)) "; containing NA/NaN values!"
  ))

  if (length(x) == 0L)
    return(invisible(x))

  # Remove the dimnames names to prevent them from printing
  xp <- x
  names(dimnames(xp)) <- NULL
  print.default(unclass(xp[row_idx, col_idx, drop = FALSE]))

  # Print footer
  if (row_trunc && col_trunc) {
    .msg_info(.frmt_big(nrow(x) - n), " more", row_name, " and ", .frmt_big(ncol(x) - n_col), " more", col_name)
    if (advice)
      .msg_info("Use `print(n = ..., n_col = ...)` to see more", row_name, " and", col_name, " respectively")
  } else if (row_trunc) {
    .msg_info(.frmt_big(nrow(x) - n), " more", row_name)
    if (advice)
      .msg_info("Use `print(n = ...)` to see more", row_name)
  } else if (col_trunc) {
    .msg_info(.frmt_big(ncol(x) - n_col), " more", col_name)
    if (advice)
      .msg_info("Use `print(n_col = ...)` to see more", col_name)
  }

  invisible(x)
}

# Contrary to base R we set 'drop = F' mirroring tibble:::`[.tbl_df`
##' @export
`[.mtrx` <- function(x, i, j, drop = FALSE, ...) {
  # Remove class() to prevent potential NextMethod() infinite loops
  x <- unclass(x)

  # Discern between being missing and being omitted entirely to mimic default behavior
  n_arg <- nargs() - !missing(drop)
  if (n_arg < 3L) {
    res <- x[i]
  } else {
    res <- x[i, j, drop = drop]
  }

  if (is.matrix(res))
    class(res) <- c("mtrx", class(res))
  res
}

##' @export
str.mtrx <- function (object, ...) {
  utils::str(unclass(object), ...)
}

.max_print_col <- function(mat, row_idx = NULL, width = getOption("width", 80L)) {
  stopifnot(is.matrix(mat))
  if (!missing(row_idx) && is.null(row_idx))
    row_idx <- rlang::missing_arg()
  width <- as.integer(width)

  if (ncol(mat) <= 1L)
    return(1L)

  # Reduce size of matrix to the maximum possible number of columns
  n_max <- max(1L, as.integer(floor(width / 2L)) - 1L)
  if (ncol(mat) > n_max) {
    mat <- mat[row_idx, seq_len(n_max), drop = FALSE]
  } else {
    mat <- mat[row_idx, , drop = FALSE]
  }

  .max_nchar <- function(x) {
    max(nchar(format(x, trim = TRUE)))
  }

  if (nrow(mat) == 0L) {
    rname_len <- 4L
  } else if (is.null(rownames(mat))) {
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

  if (nrow(mat) == 0L) {
    col_widths <- cname_len
  } else {
    # Add 1 for space between columns and 2 for the quotes around character values
    cval_len <- apply(mat, 2, .max_nchar) + 1L
    if (is.character(mat))
      cval_len <- cval_len + 2L

    # Check if col name or col value is widest
    col_widths <- pmax.int(cname_len, cval_len)
  }

  # Add row name width to first element
  col_widths[1L] <- col_widths[1L] + rname_len

  # Return the index of the last column that fits within width
  which_fit <- which(cumsum(col_widths) < width)
  max(which_fit, 1L)
}

.namesdimnames <- function(x) {
  default <- c("rows", "columns")
  ndn <- names(dimnames(x))
  if (length(ndn) > 2L)
    stop("Unexpected!")

  if (is.null(ndn)) {
    default
  } else {
    no_name <- !rlang::have_name(dimnames(x))
    if (any(no_name))
      ndn[no_name] <- default[which(no_name)]
    ndn
  }
}

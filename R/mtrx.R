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
  if (is.null(n))
    n <- getOption("print_min", 10L)
  if (is.null(n_col))
    n_col <- .max_print_col(x, row_idx = seq_len(n))

  row_trunc <- isTRUE(n < nrow(x))
  col_trunc <- isTRUE(n_col < ncol(x))
  if (!row_trunc)
    n <- nrow(x)
  if (!col_trunc)
    n_col <- ncol(x)

  # Fetch and remove dim names to prevent them from printing
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

  xp <- x
  names(dimnames(xp)) <- NULL
  print(unclass(xp[seq_len(n), seq_len(n_col), drop = FALSE]), ...)

  if (row_trunc)
    .msg_info(nrow(x) - n, " more", row_name)

  if (row_trunc && col_trunc) {
    .msg_info("Use `print(n = ..., n_col = ...)` to see more", row_name, " and", col_name, " respectively.")
  } else if (row_trunc) {
    .msg_info("Use `print(n_col = ...)` to see more", row_name)
  } else if (col_trunc) {
    .msg_info("Use `print(n = ...)` to see more", col_name)
  }

  invisible(x)
}

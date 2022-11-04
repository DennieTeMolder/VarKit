##' @export
varkit_geno <- function(x) {
  UseMethod("varkit_geno", x)
}

##' @export
varkit_geno.data.frame <- function(x) {
  x <- t(x)
  class(x) <- c("varkit_geno", "matrix", "array")
  x
}

##' @export
varkit_geno.matrix <- function(x) {
  class(x) <- c("varkit_geno", "matrix", "array")
  x
}

##' @export
t.varkit_geno <- function(x) {
  class(x) <- c("matrix", "array")
  t(x)
}

##' @export
is_varkit_geno <- function(x) {
  inherits(x, "varkit_geno")
}

##' @export
print.varkit_geno <- function(x, ..., n = NULL) {
  n_col <- ncol(x)
  cat("# A", class(x[1]), "matrix\n")

  if (is.null(n)) {
    print_subset <- x[, seq_len(min(c(100, n_col)))]
    col_widths <- .get_print_width(print_subset)
    which_fit <- which(col_widths < getOption("width"))
    idx <- ifelse(length(which_fit) == 0, 1, max(which_fit))
    print(print_subset[, seq_len(idx)], ...)
  } else {
    print(x[, seq_len(min(c(n, n_col)))], ...)
  }

  cat("# With a total of", n_col, "sites x", nrow(x), "samples and",
       sum(is.na(x)), "missing values\n")

  if (is.null(n) && idx < n_col)
    cat("# Use `print(n = ...)` to see more sites\n")

  invisible(x)
}

.maxnchar <- function(x) {
  max(nchar(x), na.rm = TRUE)
}

.get_print_width <- function(x) {
  stopifnot(is.matrix(x))

  if (is.null(rownames(x))) {
    rname_len <- nchar(nrow(x)) + 3
  } else {
    rname_len <- .maxnchar(rownames(x))
  }

  if (is.null(colnames(x))) {
    # We add 3 for the [,] and for the space before the column
    cname_len <- nchar(seq_len(ncol(x))) + 4
  } else {
    # We add 2 here for the potential `` around var names
    cname_len <- nchar(colnames(x)) + 3
  }

  col_len <- apply(x, 2, .maxnchar) + 1
  if (is.character(x[1]))
    col_len <- col_len + 2

  # Check if col name or col value is widest, add row name width to first element
  col_widths <- pmax.int(cname_len, col_len)
  col_widths[1] <- col_widths[1] + rname_len
  cumsum(col_widths)
}

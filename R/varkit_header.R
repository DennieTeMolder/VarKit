##' @export
varkit_header <- function(x) {
  stopifnot(is.character(x))
  stopifnot(length(x) >= 2 )

  if (!stringr::str_detect(x[1], "^##fileformat=VCF"))
    warning("No valid VCF fileformat meta information detected on the first line!")

  if (!all(stringr::str_starts(x, "#")))
    stop("Invalid VCF. Not all meta lines start with a '#'!")

  header <- list(
    meta = .parse_meta(utils::head(x, n = -1)),
    col_names = .parse_col_names(utils::tail(x, n = 1))
  )
  class(header) <- c("varkit_header", "list")
  header
}

.parse_meta <- function(x) {
  x <- stringr::str_remove(x, "##")
  x <- stringr::str_split_fixed(x, "=", n = 2)
  tibble::tibble(name = x[, 1], value = x[, 2])
}

.parse_col_names <- function(x) {
  if (substr(x, 2, 2) == "#")
    stop("The last line does not appear to be the header line")
  col_names <- stringr::str_remove(x, "#")
  col_names <- stringr::str_split(col_names, "\t")[[1]]

  # Standardise the names of fixed columns
  fixed_names <- .fixed_columns()
  fixed_idx <- stats::na.omit(match(fixed_names, tolower(col_names)))
  col_names[fixed_idx] <- fixed_names[seq_along(fixed_names) %in% fixed_idx]
  col_names
}

##' @export
is_varkit_header <- function(x) {
  inherits(x, "varkit_header")
}

##'@export
print.varkit_header <- function(x, ..., n = NULL) {
  cat("*VarKit Header Object*\n")
  if (!is.null(meta(x))) {
    cat("Header metadata, `meta(header)`:\n")
    print(meta(x), ..., n = n)
  }
  cat("\nColumn names (for 'skip_cols'), `header$col_names`:\n")
  print(x$col_names, ...)
  cat("# Use `samples(header)` to get only columns recognised as samples\n")
}

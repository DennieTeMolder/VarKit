##' @export
varkit_header <- function(x) {
  stopifnot(is.character(x))

  # The last line of the header should be the only line with a single "#"
  header_end <- stringr::str_detect(x, "^#[^#\n]")
  if (!any(header_end)) {
    warning("No VCF header line detected! Using first line for column names.")
    header_end <- 1
  } else {
    header_end <- which(header_end)[1]
  }

  if (header_end > 1) {
    x <- utils::head(x, n = header_end)
    if (!all(stringr::str_starts(x, "#")))
      stop("Not all lines above the header line start with a '#'!")

    # The last line of the header contains the column names, the rest is metadata
    meta <- .parse_meta(utils::head(x, n = -1))
    col_names <- .parse_col_names(utils::tail(x, n = 1))
  } else {
    meta <- NULL
    col_names <- .parse_col_names(x[header_end])
  }

  header <- list(
    meta = meta,
    col_names = col_names
  )
  class(header) <- c("varkit_header", "list")
  header
}

.parse_meta <- function(x) {
  stopifnot(is.character(x))
  x <- stringr::str_remove(x, "##")
  x <- stringr::str_split_fixed(x, "=", n = 2)
  tibble::tibble(name = x[, 1], value = x[, 2])
}

.parse_col_names <- function(x) {
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

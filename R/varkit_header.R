##' @export
varkit_header <- function(x) {
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
    meta <- varkit_meta(utils::head(x, n = -1))
    col_names <- .parse_col_names(utils::tail(x, n = 1))
  } else {
    meta <- NULL
    col_names <- .parse_col_names(x[header_end])
  }

  header <- list(
    meta = meta,
    col_names = col_names
  )
  class(header) <- "varkit_header"
  header
}

##'@export
print.varkit_header <- function(x, ..., n = 10, verbose = TRUE) {
  print(x$meta, ..., n = n, verbose = verbose)
  cat("\nColumn names (for 'skip_cols'):\n")
  print(x$col_names, ...)
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

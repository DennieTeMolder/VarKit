##' @export
varkit_header <- function(x) {
  # The last line of the header should be the only line with a single "#"
  header_end <- stringr::str_detect(x, "^#[^#\n]")
  if (!any(header_end))
    stop("No header line detected!")
  header_end <- which(header_end)[1]

  x <- head(x, n = header_end)
  if (!all(stringr::str_starts(x, "#")))
    stop("Not all lines above the header line start with a '#'!")

  # The last line of the header contains the column names, the rest is metadata
  meta <- varkit_meta(head(x, n = -1))
  col_names <- .parse_col_names(tail(x, n = 1))

  header <- list(
    meta = meta,
    col_names = col_names
  )
  class(header) <- "varkit_header"
  header
}

##'@export
print.varkit_header <- function(x, n = 10, verbose = TRUE) {
  print(x$meta, n = n, verbose = verbose)
  cat("\nColumn names (for 'skip_cols'):\n")
  print(x$col_names)
}

##' @export
find_header_line <- function(file) {
  if (!.is_connection(file)) {
    file <- file(file, "r")
    .defer(close(file))
  }

  i <- 0L
  while (length(line <- readLines(file, n = 1L)) > 0L) {
    i <- i + 1L
    if (!isTRUE(startsWith(line, "##")))
      return(i)
  }
  0L
}

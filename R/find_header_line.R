##' @export
find_header_line <- function(file) {
  con <- file(file, "r")
  on.exit(close(con))
  i <- 0L
  while (length(line <- readLines(con, n = 1L)) > 0L) {
    i <- i + 1L
    if (!isTRUE(startsWith(line, "##")))
      return(i)
  }
  0L
}

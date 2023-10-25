##' @export
find_header_line <- function(file) {
  con <- file(file, "r")
  on.exit(close(con))
  i <- 0L
  while (length(line <- readLines(con, n = 1)) > 0) {
    i <- i + 1L
    if (!isTRUE(.str_starts(line, "##")))
      return(i)
  }
  0L
}

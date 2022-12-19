##' @export
read_vcf_header <- function(file) {
  .stopifnotsingle(file, class = "character")
  header_end <- .find_header_end(file)
  header <- readr::read_lines(file, n_max = header_end)
  varkit_header(header)
}

.find_header_end <- function(file) {
  n <- 0
  con <- file(file, "r")
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if (substr(line, 1, 1) != "#")
      stop("Invalid VCF. Not all meta lines start with a '#' or the header line is missing!")
    n <- n + 1
    if (substr(line, 2, 2) != "#") {
      close(con)
      return(n)
    }
  }
  close(con)
  stop("Invalid VCF. The header line could not be detected!")
}

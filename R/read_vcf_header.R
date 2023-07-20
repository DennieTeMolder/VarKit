##' @export
read_vcf_header <- function(file) {
  stopifnot(file.exists(file))
  header_end <- .find_header_end(file)
  header <- readr::read_lines(file, n_max = header_end)
  varkit_header(header)
}

.find_header_end <- function(file) {
  n <- 0
  con <- file(file, "r")
  on.exit(close(con))
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    n <- n + 1
    if (substr(line, 1, 1) != "#")
      stop("Invalid VCF. Not all meta lines start with a '#' or the header line is missing!")
    if (substr(line, 2, 2) != "#")
      return(n)
  }
  stop("Invalid VCF. The header line could not be detected!")
}

##' @export
find_header_line <- function(file) {
  file <- path.expand(file)
  if (!file.exists(file))
    stop("Could not locate the provided file: '", file, "'")
  .find_header_lineC(file)
}

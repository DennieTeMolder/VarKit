##' @export
read_vcf_header <- function(path) {
  header <- readr::read_lines(path, n_max = 1e4)

  # Check if the first line contains the VCF file spec
  if (!stringr::str_detect(header[1], "^##fileformat=VCF"))
    warning("No valid VCF fileformat header detected on the first line!")

  # We only read a limited number of lines, check if it contains the full header
  if (stringr::str_starts(tail(header, n = 1), "##"))
    stop("VCF header exceeds maximum line count!")

  # The last line of the header should be the only line with a single "#"
  header_end <- stringr::str_detect(header, "^#[^#\n]")
  if (!any(header_end))
    stop("No header line detected!")
  header_end <- which(header_end)[1]

  # Parse colnames and keep remainder
  col_names <- .parse_col_names(header[header_end])
  header <- head(header, n = header_end - 1)

  # Parse the rest of the header
  meta <- varkit_meta(header)

  header <- list(
    meta = meta,
    col_names = col_names
  )
  class(header) <- "varkit_header"
  header
}

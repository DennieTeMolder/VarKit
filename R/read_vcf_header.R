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
  header <- .parse_meta_lines(header)
  info <- .parse_meta_type(header, type = "INFO")
  form <- .parse_meta_type(header, type = "FORMAT")
  filter <- .parse_meta_type(header, type = "FILTER")
  alt <- .parse_meta_type(header, type = "ALT")
  contig <- .parse_meta_type(header, type = "contig")

  # Split off filespec and skipped parsed types
  spec <- header$value[1]
  header <- dplyr::filter(
    header[-1, ],
    !name %in% c("INFO", "FORMAT", "FILTER", "ALT", "contig")
  )

  list(
    spec = spec,
    info = info,
    format = form,
    filter = filter,
    alt = alt,
    contig = contig,
    other = header,
    col_names = col_names
  )
}

##' @export
read_vcf_header <- function(file, n_max = 1e5, ...) {
  header <- readr::read_lines(file, n_max = n_max, ...)

  # Check if the first line contains the VCF file spec
  if (!stringr::str_detect(header[1], "^##fileformat=VCF"))
    warning("No valid VCF fileformat header detected on the first line!")

  # We only read a limited number of lines, check if it contains the full header
  if (stringr::str_starts(utils::tail(header, n = 1), "##"))
    stop("VCF header exceeds maximum line count! Try increasing 'n_max'.")

  varkit_header(header)
}

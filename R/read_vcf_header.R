##' @export
read_vcf_header <- function(path) {
  header <- readr::read_lines(path, n_max = 1e4)

  # Check if the first line contains the VCF file spec
  if (!stringr::str_detect(header[1], "^##.*VCF"))
    stop("No valid VCF header detected")

  # We only read a limited number of lines, check if it contains the full header
  if (stringr::str_starts(header[length(header)], "#"))
    stop("VCF header exceeds maximum line count!")

  # The last line of the header should be the only line with a single "#"
  header_end <- which(stringr::str_detect(header, "^#[^#\n]"))[1]
  col_names <- stringr::str_remove(header[header_end], "#")
  col_names <- stringr::str_split(col_names, "\t")[[1]]

  if (length(col_names) < 5)
    stop("The number of columns is too low, make sure the file is TAB delimited.")

  # Standardise the names of fixed columns
  fixed_names <- .fixed_columns()
  fixed_idx <- na.omit(match(fixed_names, tolower(col_names)))
  col_names[fixed_idx] <- fixed_names[seq_along(fixed_names) %in% fixed_idx]

  # Parse the rest of the header
  header <- header[seq(1, header_end - 1)]
  header <- stringr::str_remove(header, "##")
  header <- stringr::str_split_fixed(header, "=", n = 2)
  colnames(header) <- c("name", "value")
  header <- tibble::as_tibble(header)

  list(
    spec = header$value[1],
    header = header[-1, ],
    col_names = col_names
  )
}

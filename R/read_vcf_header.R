##' @export
read_vcf_header <- function(file, max_lines = 1e6) {
  header <- .read_vcf_header_internal(file, max_lines = max_lines)
  new_varkit_header(
    meta = .parse_header_meta(header$meta),
    col_names = .parse_header_line(header$header_line)
  )
}

.read_vcf_header_internal <- function(file, max_lines) {
  if (.is_connection(file)) {
    if (isSeekable(file) && seek(file) != 0L) {
      warning(
        "The provided connection is not at the start of 'file'.",
        "If this was not intentional the header might be malformed.", call. = FALSE
      )
    }
  } else {
    file <- file_con(file, "r")
    .defer(close(file))
  }

  meta_lines <- vector("character", length = max_lines)
  for (i in seq_along(meta_lines)) {
    line <- readLines(file, n = 1L)
    if (!isTRUE(startsWith(line, "##")))
      break()
    meta_lines[[i]] <- line
  }

  if (length(line) < 1L || !stringr::str_detect(line, "^#[^#]"))
    stop("After reading ", i - 1L, " lines: expected the VCF header line!", call. = FALSE)

  meta_lines <- meta_lines[seq_len(i - 1L)]

  list(
    meta = meta_lines,
    header_line = line
  )
}

.parse_header_meta <- function(meta) {
  if (!isTRUE(startsWith(meta[1L], "##fileformat=VCF")))
    warning("No valid VCF 'fileformat' description detected on the first line!", call. = FALSE)
  if (length(meta) == 0L)
    return(NULL)
  meta <- stringr::str_remove(meta, "^##")
  meta <- stringr::str_split_fixed(meta, "=", n = 2L)
  tibble::tibble(name = meta[, 1L], value = meta[, 2L])
}

.parse_header_line <- function(header_line) {
  col_names <- stringr::str_split_1(
    stringr::str_remove(header_line, "^#"),
    pattern = stringr::fixed("\t")
  )

  # Standardise the names of fixed columns
  lower <- tolower(col_names)
  .if_else(lower %in% .fixed_columns(), lower, col_names)
}

##' @export
read_vcf_header <- function(file) {
  header <- .read_vcf_header_internal(file)
  new_varkit_header(
    meta = .parse_meta(header$meta),
    col_names = .parse_col_names(header$col_names)
  )
}

.read_vcf_header_internal <- function(file) {
  if (inherits(file, "connection")) {
    if (!is_open(file))
      stop("The provided 'file' connection is invalid!", call. = FALSE)
    con <- file
    file <- summary(con)$description
  } else {
    con <- file(file, "r")
    on.exit(close(con))
  }

  header_linum <- find_header_line(file)
  if (header_linum < 1)
    stop("Could not detect header line! Check the output of `find_header_line(file)`.", call. = FALSE)

  meta <- character()
  if (header_linum > 1)
    meta <- readLines(con, n = header_linum - 1)

  col_names <- .scan_fields(con, nlines = 1)
  list(meta = meta, col_names = col_names)
}

.parse_meta <- function(meta) {
  if (is.null(meta))
    return(meta)
  if (!.str_starts(meta[1], "##fileformat=VCF"))
    warning("No valid VCF 'fileformat' description detected on the first line!", call. = FALSE)
  meta <- stringr::str_remove(meta, "^##")
  meta <- stringr::str_split_fixed(meta, "=", n = 2)
  tibble::tibble(name = meta[, 1], value = meta[, 2])
}

.parse_col_names <- function(col_names) {
  col_names[1] <- stringr::str_remove(col_names[1], "^#")

  # Standardise the names of fixed columns
  fixed_names <- .fixed_columns()
  fixed_idx <- stats::na.omit(match(fixed_names, tolower(col_names)))
  col_names[fixed_idx] <- fixed_names[seq_along(fixed_names) %in% fixed_idx]
  col_names
}

##' @export
read_vcf_header <- function(file) {
  header <- .read_vcf_header_internal(file)
  new_varkit_header(
    meta = .parse_meta(header$meta),
    col_names = .parse_header_line(header$header_line)
  )
}

.read_vcf_header_internal <- function(file, max_lines = 1e5) {
  if (inherits(file, "connection")) {
    if (!is_open(file))
      stop("The provided 'file' connection is invalid!", call. = FALSE)
    if (isSeekable(file) && seek(file) != 0)
      warning("The provided connection is not at the start of 'file'\n",
              ("If this was not intentional, the header will be incorrect"))
    con <- file
  } else {
    con <- file(file, "r")
    on.exit(close(con))
  }

  meta_lines <- vector("list", length = max_lines)
  for (i in seq_along(meta_lines)) {
    line <- readLines(con, n = 1)
    if (!isTRUE(.str_starts(line, "##")))
      break()
    meta_lines[[i]] <- line
  }

  if (length(line) < 1 || .str_starts(line, "##"))
    stop("At line ", i, ": could not detect header line!")

  list(
    meta = unlist(meta_lines),
    header_line = line
  )
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

.parse_header_line <- function(header_line) {
  col_names <- stringr::str_split_1(
    stringr::str_remove(header_line, "^#"),
    pattern = stringr::fixed("\t")
  )

  # Standardise the names of fixed columns
  fixed_names <- .fixed_columns()
  fixed_idx <- match(fixed_names, tolower(col_names))
  fixed_idx <- fixed_idx[!is.na(fixed_idx)]
  col_names[fixed_idx] <- fixed_names[seq_along(fixed_names) %in% fixed_idx]
  col_names
}


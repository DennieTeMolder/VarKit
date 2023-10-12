##' @export
index_vcf <- function(file, header = NULL, chunk_size = 1e5, ...) {
  stopifnot(file.exists(file))
  if (!is.null(header)) {
    stopifnot(is_varkit_header(header))
  } else {
    cli::cli_progress_step("Parsing VCF header")
    header <- read_vcf_header(file)
    cli::cli_progress_done()
  }
  if (!"chrom" %in% col_names(header))
    stop("Could not detect CHROM field in VCF! Examine the output of `read_vcf_header()`.")

  spec <- spec_vcf(file = file, col_names = col_names(header), ...)

  # Override spec to ensure all lines are retained
  spec_chrom <- spec
  spec_chrom$col_types <- readr::cols_only(chrom = "f")
  spec_chrom$skip_empty_rows <- FALSE
  spec_chrom$comment <- ""
  spec_chrom$na <- character()
  spec_chrom$skip <- nrow(header$meta) + 1
  spec_chrom$lazy <- NULL
  spec_chrom$chunk_size <- chunk_size
  spec_chrom$callback <- readr::DataFrameCallback$new(function(chunk, start) {
    which_keep <- which(!.is_repeated(chunk$chrom))
    tibble::tibble(
      id = as.character(chunk$chrom[which_keep]),
      linum = which_keep + start - 1,
      chunk_end = nrow(chunk) + start - 1
    )
  })

  cli::cli_progress_step("Indexing VCF body")
  index <- do.call(readr::read_delim_chunked, spec_chrom)
  total_lines <- utils::tail(index$chunk_end, n = 1)
  index <- index[!.is_repeated(index$id), 1:2]
  index$n_lines <- diff(c(index$linum, total_lines + 1))
  index$linum <- index$linum + spec_chrom$skip

  # Checks
  if (any(nchar(index$id) == 0))
    warning("Some CHROM fields appear to be empty.")
  if (any(substr(index$id, 1, 1) == "#"))
    warning("The VCF contains comments. The index likely requires manual filtering.")
  if (anyDuplicated(index$id) != 0)
    warning("The VCF appears to be unsorted, this could cause errors later.")

  # Create index and add additional properties for next_chrom()
  varkit_index(index, file = file, header = header, spec = spec)
}

##' @export
index_vcf_partial <- function(file, chunk_size = 5e3, header = NULL, ...) {
  if (!is.null(header)) {
    stopifnot(is_varkit_header(header))
  } else {
    cli::cli_progress_step("Parsing VCF header")
    header <- read_vcf_header(file)
    cli::cli_progress_done()
  }
  spec <- spec_vcf(file = file, col_names = col_names(header), ...)
  index <- tibble::tibble(id = "chunk_size", linum = nrow(meta(header)) + 2, n_lines = chunk_size)
  varkit_index(index, file = file, header = header, spec = spec)
}

## Unused ------
index_vcf_slow <- function(file, ...) {
  con <- file(file, "r")

  cli::cli_progress_step("Scanning VCF header")
  line <- readLines(con, n = 1, warn = FALSE)
  if (substr(line, 1, 16) != "##fileformat=VCF")
    warning("No valid VCF fileformat tag detected on the first line.")

  # Skip rest of header header
  n <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    n <- n + 1
    if (substr(line, 1, 2) != "##")
      break()
  }
  cli::cli_progress_done()

  # Checks
  if (length(line) == 0) {
    close(con)
    stop("Reached end of VCF without detecting the header line!")
  }
  if (toupper(substr(line, 1, 7)) != "#CHROM\t") {
    close(con)
    stop("The first field/column of the header should be CHROM (line ", n ,")!")
  }

  # Index variants by line
  cli::cli_progress_step("Indexing... [{round(n / 1e6, 1)}M lines]", "Indexed {n} lines")
  current <- ""
  offset <- seek(con)
  index <- list(id = NULL, linum = NULL)
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    n <- n + 1
    if (n %% 1e5 == 0) cli::cli_progress_update()
    # REVIEW should comment lines be skipped or reported some other way?
    chrom <- substr(line, 1, regexpr("\t", line) - 1)
    if (chrom != current) {
      current <- chrom
      index$id <- c(index$id, chrom)
      index$linum <- c(index$linum, n)
    }
  }
  close(con)
  cli::cli_progress_done()

  if (is.null(index$id))
    stop("No variants found in VCF file!")
  if ("" %in% index$id)
    warning("Index contains empty lines.")
  if (any(duplicated(index$id)))
    warning("The VCF appears to be unsorted, this could cause errors later.")

  # Add end of file
  index$n_lines <- diff(c(index$linum, n + 1))

  header <- varkit_header(readLines(file, index$linum[1] - 1))
  spec <- spec_vcf(file = file, col_names = col_names(header), ...)

  varkit_index(
    tibble::as_tibble(index),
    file = file,
    header = header,
    spec = spec
  )
}

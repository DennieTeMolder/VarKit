##' @export
index_vcf <- function(file, ...) {
  n <- 1
  con <- file(file, "r")

  cli::cli_progress_step("Scanning VCF header")
  line <- readLines(con, n = 1, warn = FALSE)
  if (substr(line, 1, 16) != "##fileformat=VCF")
    warning("No valid VCF fileformat tag detected on the first line.")

  # Skip rest of header header
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
  index$id <- c(index$id, "EOF")
  index$linum <- c(index$linum, n + 1)

  index <- varkit_index(tibble::as_tibble(index), file = file)
  props <- .get_props(index)
  props$header <- varkit_header(readLines(file, index$linum[1] - 1))
  props$spec <- spec_vcf(file, props$header$col_names, ...)
  index
}

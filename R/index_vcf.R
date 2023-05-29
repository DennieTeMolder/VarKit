##' @export
index_vcf <- function(file) {
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
  index <- list(chrom = NULL, start = NULL)
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    n <- n + 1
    if (n %% 1e5 == 0) cli::cli_progress_update()
    # REVIEW should comment lines be skipped or reported some other way?
    chrom <- substr(line, 1, regexpr("\t", line) - 1)
    if (chrom != current) {
      current <- chrom
      index$chrom <- c(index$chrom, chrom)
      index$start <- c(index$start, n)
    }
  }
  cli::cli_progress_done()
  close(con)

  if (is.null(index$chrom))
    stop("No variants found in VCF file!")
  if ("" %in% index$chrom)
    warning("Index contains empty lines.")
  if (any(duplicated(index$chrom)))
    warning("The VCF appears to be unsorted, this could cause errors later.")

  index$n <- diff(c(index$start, n + 1))
  tibble::as_tibble(index)
}

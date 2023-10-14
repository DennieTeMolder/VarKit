##' @export
compute_vcf_index <- function(file) {
  file <- path.expand(file)
  stopifnot(file.exists(file))
  tibble::as_tibble(.index_vcfC(file))
}

##' @export
index_vcf <- function(file, ...) {
  index <- compute_vcf_index(file)
  if (nrow(index) < 2) {
    stop("The file does not appear to be a VCF or tabular data of any sort!")
  }

  index$linum <- cumsum(c(1L, utils::head(index$n_lines, n = -1)))
  index <- index[c(1, 3, 2)]

  # Header line row-index
  h_idx <- 2
  if (index$id[1] != "##") {
    if (index$n_lines[1] != 1)
      stop("The file is missing a header or column names!")
    warning("No meta lines detected, using the first line as header.")
    h_idx <- 1
  }

  if (!stringr::str_detect(tolower(index$id[h_idx]), "^#?chrom$"))
    stop("At line ", index$linum[h_idx], ": Expected a header line with CHROM as the first field!")
  if (index$n_lines[h_idx] != 1L)
    stop("At line ", index$linum[h_idx], ": Expected a single a header line, but found ", index$n_lines[h_idx], " instead!")

  # Remove meta and header lines from index (these are implicitly stored in linum)
  index <- utils::tail(index, n = -h_idx)

  # Check body
  if (any(nchar(index$id) == 0))
    warning("Some CHROM fields appear to be empty.")
  if (any(substr(index$id, 1, 1) == "#"))
    warning("The VCF contains comments. The index likely requires manual filtering.")
  if (anyDuplicated(index$id) != 0)
    warning("The VCF appears to be unsorted, this could cause errors later.")

  # Required for next_chrom()
  header <- read_vcf_header(file)
  if (!"chrom" %in% col_names(header))
    stop("Could not detect CHROM field in VCF! Examine the output of `read_vcf_header()`.")
  spec <- spec_vcf(file = file, col_names = col_names(header), ...)

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

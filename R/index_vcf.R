##' @export
index_vcf_header <- function(file, chunk_size = 1e4, ...) {
  index <- tibble::tibble(
    id = "chunk_size",
    linum = find_header_line(file) + 1,
    n_lines = chunk_size
  )
  varkit_index(index = index, file = file, ...)
}

##' @export
index_vcf <- function(file, size_bp = -1, chunk_size = 1e5, ...) {
  stopifnot(file.exists(file))
  stopifnot(is.numeric(size_bp), length(size_bp) == 1L)
  stopifnot(is.numeric(chunk_size), length(chunk_size) == 1L, chunk_size > 1)

  # Loop over the file in chunks to index it
  index <- .index_vcf_internal(file = file, size_bp = size_bp, chunk_size = chunk_size)

  cli::cli_progress_step("Collecting results")

  # Combine adjacent values that were split over multiple chunks
  index$n <- cumsum(!duplicated(index$values))
  index <- dplyr::summarise(
    dplyr::group_by(index, .data$n),
    id = .data$values[1],
    linum = min(.data$linum),
    n_lines = sum(.data$lengths)
  )[-1]

  # Warn user about undesirable index properties
  if (anyNA(index$id))
    warning("Some CHROM fields appear to be empty/NA. The index requires manual filtering.")
  if (anyDuplicated(index$id) != 0)
    warning("The VCF appears to be unsorted, this could cause errors later.")

  varkit_index(index = index, file = file, ...)
}

.index_vcf_internal <- function(file, size_bp, chunk_size, max_results = 1e5) {
  use_pos <- size_bp >= 2

  # Open the file for reading
  file <- file(file, "r")
  on.exit(close(file))

  cli::cli_progress_step("Reading VCF header")
  header <- read_vcf_header(file)
  col_names <- header$col_names
  header_line <- nrow(meta(header)) + 1L
  rm(header)

  if (!any(col_names == "chrom"))
    stop("Could not detect the CHROM column! Check the output of `read_vcf_header(file)`.", call. = FALSE)
  if (use_pos && !any(col_names == "pos"))
    stop("Could not detect the POS column! Check the output of `read_vcf_header(file)`.", call. = FALSE)

  # Select only the necessary columns of the VCF body
  col_types <- c(chrom = "c", pos = if (use_pos) "i" else NULL, .default = "_")

  # Report progress during indexing
  total <- 0L
  frmt <- function(x) .frmt_big(x)
  cli::cli_progress_step("Indexing VCF body... [{frmt(total)} lines]", "Indexed {frmt(total)} lines")

  # Loop over the VCF body in chunks and covert to Run Length Encoding (RLE)
  results <- vector("list", max_results)
  i <- 1L
  repeat {
    tbl <- read_tbl(file,
      col_names = col_names,
      nrows = chunk_size,
      na.strings = c("", "."),
      col_types = col_types,
      sep = "\t",
      header = FALSE,
      check.names = FALSE,
      quote = "",
      as.is = TRUE,
      fill = FALSE,
      strip.white = TRUE,
      comment.char = "#"
    )

    n_row <- nrow(tbl)
    if (n_row < 1)
      break()

    # Bin positions based on size_bp
    if (use_pos) {
      tbl <- paste0(tbl$chrom, ":", tbl$pos %/% size_bp)
    } else {
      tbl <- tbl$chrom
    }

    results[[i]] <- do.call(tibble::tibble, rle(tbl))
    rm(tbl)

    # Update progress bar
    total <- total + n_row
    cli::cli_progress_update()

    i <- i + 1L
    if (i == max_results)
      stop("Surpassed the maximum number of iterations before reaching the end of file! Increase 'chunk_size'.", call. = FALSE)
  }

  if (i == 1)
    stop("Could not detect the VCF body, check if the 'file' is a valid VCF!", call. = FALSE)

  cli::cli_progress_done()

  results <- do.call(rbind, results[seq_len(i - 1L)])

  # Add the line number column
  results$linum <- cumsum(c(
    header_line + 1L,
    utils::head(results$lengths, n = -1)
  ))

  results
}

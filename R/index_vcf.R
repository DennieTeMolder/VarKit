##' @export
index_vcf <- function(file, size_bp = -1, chunk_size = 1e4, ...) {
  .stopifnotsingle(size_bp, what = is.numeric)
  .stopifnotsingle(chunk_size, what = is.numeric)
  stopifnot(chunk_size > 1)

  # Loop over the file in chunks to index it
  index <- .index_vcf_internal(file = file, size_bp = size_bp, chunk_size = chunk_size)

  .msg_step("Collecting results")

  # Combine adjacent values that were split over multiple chunks
  index$n <- cumsum(!duplicated(index$values))
  index <- dplyr::summarise(
    dplyr::group_by(index, .data$n),
    id = .data$values[1],
    linum = min(.data$linum),
    n_lines = sum(.data$lengths)
  )[-1L]

  # Warn user about undesirable index properties
  if (.any_na(index$id))
    warning("Some CHROM fields appear to be empty/NA. The index requires manual filtering.")
  if (anyDuplicated(index$id) != 0)
    warning("The VCF appears to be unsorted, this could cause errors later.")

  varkit_index(index = index, file = file, ...)
}

.index_vcf_internal <- function(file, size_bp, chunk_size, max_results = 1e5) {
  use_pos <- size_bp >= 1

  # Open the file for reading
  file <- file_con(file, "r")
  .defer(close(file))

  .msg_step("Reading VCF header")
  header <- read_vcf_header(file)
  col_names <- header$col_names
  header_line <- nrow(meta(header)) + 1L
  rm(header)

  if (!any(col_names == "chrom"))
    stop("Could not detect the CHROM column! Check the output of `read_vcf_header(file)`.", call. = FALSE)
  if (use_pos && !any(col_names == "pos"))
    stop("Could not detect the POS column! Check the output of `read_vcf_header(file)`.", call. = FALSE)

  # Select only the necessary columns of the VCF body
  col_types <- c(chrom = "c", pos = if (use_pos) "i" else "_", .default = "_")

  # Report progress during indexing
  frmt <- function(x) .frmt_big(as.integer(x))
  cli::cli_progress_step(
    "Indexing VCF body... {frmt(cli::pb_current)} lines completed @{frmt(cli::pb_rate_raw)}/s [{cli::pb_elapsed}]",
    msg_done = "Indexed VCF body, {frmt(cli::pb_current)} lines total",
    msg_failed = "Indexing VCF body failed after {frmt(cli::pb_current)} lines"
  )
  cli::cli_progress_update(set = 0L, force = TRUE)

  # Loop over the VCF body in chunks and covert to Run Length Encoding (RLE)
  results <- vector("list", max_results)
  i <- 1L
  repeat {
    tbl <- read_tbl(file,
      col.names = col_names,
      nrows = chunk_size,
      na.strings = c("", "."),
      colClasses = col_types,
      sep = "\t",
      header = FALSE,
      check.names = FALSE,
      quote = "",
      as.is = TRUE,
      fill = FALSE,
      strip.white = TRUE,
      blank.lines.skip = TRUE,
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
    cli::cli_progress_update(inc = n_row)

    i <- i + 1L
    if (i == max_results)
      stop("Surpassed the maximum number of iterations before reaching the end of file! Increase 'chunk_size'.", call. = FALSE)
  }

  if (i == 1L)
    stop("Could not detect the VCF body, check if the 'file' is a valid VCF!", call. = FALSE)

  cli::cli_progress_done()

  results <- do.call(rbind, results[seq_len(i - 1L)])

  # Add the line number column
  results$linum <- cumsum(c(
    header_line + 1L,
    utils::head(results$lengths, n = -1L)
  ))

  results
}

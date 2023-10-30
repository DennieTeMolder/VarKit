##' @export
index_vcf <- function(file, size_bp = -1, chunk_size = 1e5, ...) {
  stopifnot(length(size_bp) == 1L, is.numeric(size_bp))
  stopifnot(length(chunk_size) == 1L, is.numeric(chunk_size), chunk_size > 1)

  # Open the file for reading
  file_path <- file
  file <- file(file, "r")
  on.exit(close(file))

  cli::cli_progress_step("Reading VCF header")
  header <- read_vcf_header(file)

  if (!any(header$col_names == "chrom"))
    stop("Could not detect the CHROM column! Check the output of `read_vcf_header(file)`.")
  if (size_bp >= 2 && !any(header$col_names == "pos"))
    stop("Could not detect the POS column! Check the output of `read_vcf_header(file)`.")

  # Select only the necessary columns of the VCF body
  col_types <- c(chrom = "c", pos = if (size_bp >= 2) "i" else NULL, .default = "_")

  # Report progress during indexing
  total <- 0L
  frmt <- function(x) .frmt_big(x)
  cli::cli_progress_step("Indexing VCF body... [{frmt(total)} lines]", "Indexed {frmt(total)} lines")

  max_results <- 1e5L
  index <- vector("list", max_results)
  i <- 1L
  repeat {
    tbl <- read_tbl(file,
      col_names = header$col_names,
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

    if (size_bp >= 2) {
      tbl <- paste0(tbl$chrom, ":", tbl$pos %/% size_bp)
    } else {
      tbl <- tbl$chrom
    }

    index[[i]] <- do.call(tibble::tibble, rle(tbl))
    rm(tbl)

    # Update progress bar
    total <- total + n_row
    cli::cli_progress_update()

    i <- i + 1L
    if (i == max_results)
      stop("Surpassed the maximum number of iterations before reaching the end of file! Increase 'chunk_size'.")
  }

  if (i == 1)
    stop("Could not detect the VCF body, check if the 'file' is a valid VCF!")

  cli::cli_progress_step("Collecting results")
  index <- do.call(rbind, index[seq_len(i - 1L)])

  index$n <- cumsum(!duplicated(index$values))
  index <- dplyr::summarise(
    dplyr::group_by(index, .data$n),
    values = .data$values[1],
    lengths = sum(.data$lengths)
  )

  index$linum <- c(0L, cumsum(utils::head(index$lengths, n = -1))) +
    (nrow(meta(header)) + 2L)
  index <- dplyr::select(index, id = .data$values, linum = .data$linum, n_lines = .data$lengths)

  # Warn user about undesirable index properties
  if (anyNA(index$id))
    warning("Some CHROM fields appear to be empty/NA. The index requires manual filtering.")
  if (anyDuplicated(index$id) != 0)
    warning("The VCF appears to be unsorted, this could cause errors later.")

  varkit_index(index = index, file = file_path, ...)
}

##' @export
index_vcf_header <- function(file, chunk_size = 1e4, ...) {
  index <- tibble::tibble(
    id = "chunk_size",
    linum = find_header_line(file) + 1,
    n_lines = chunk_size
  )
  varkit_index(index = index, file = file, ...)
}

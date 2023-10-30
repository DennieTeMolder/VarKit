##' @export
index_vcf <- function(file, chunk_size = 1e5, ...) {
  file_path <- file
  file <- file(file, "r")
  on.exit(close(file))

  cli::cli_progress_step("Reading VCF header")
  header <- read_vcf_header(file)

  if (!any(header$col_names == "chrom"))
    stop("Could not detect the CHROM column! Check the output of `read_vcf_header(file)`.")

  cli::cli_progress_step("Indexing VCF body")
  max_results <- 1e5L
  index <- vector("list", max_results)
  i <- 1L
  repeat {
    tbl <- read_tbl(file,
      col_names = header$col_names,
      nrows = chunk_size,
      na.strings = c("", "."),
      col_types = c(chrom = "c", .default = "_"),
      sep = "\t",
      header = FALSE,
      check.names = FALSE,
      quote = "",
      as.is = TRUE,
      fill = FALSE,
      strip.white = TRUE,
      comment.char = "#"
    )

    if (nrow(tbl) < 1)
      break()

    index[[i]] <- do.call(tibble::tibble, rle(tbl$chrom))
    rm(tbl)

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

  cli::cli_progress_step("Validating index")
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

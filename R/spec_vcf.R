##' @export
spec_vcf <- function(file, col_names = NULL, skip_cols = NULL, na_geno = "./.", spec = NULL, ...) {
  if (!is.null(col_names)) {
    stopifnot(is.character(col_names))
  } else {
    cli::cli_progress_step("Parsing VCF header")
    col_names <- read_vcf_header(file)$col_names
    cli::cli_progress_done()
  }

  if (!is.null(skip_cols)) {
    stopifnot(is.character(skip_cols))
    if (any(c("chrom", "pos") %in% skip_cols))
      stop("The 'chrom' and/or 'pos' columns cannot be skipped!")
    if (!all(skip_cols %in% col_names))
      stop("Not all column names provided for 'skip_cols' are in the VCF header!")
  }
  stopifnot(is.character(na_geno))
  if (!is.null(spec))
    stopifnot(is.list(spec))

  # Default settings
  args <- list(
    delim = "\t",
    quote = "",
    escape_backslash = FALSE,
    escape_double = FALSE,
    col_types = readr::cols(
      chrom = "c",
      pos = "i",
      id = "c",
      ref = "c",
      alt = "c",
      qual = "d",
      filter = "c",
      info = "c",
      format = "c"
    ),
    quoted_na = FALSE,
    comment = "#",
    trim_ws = TRUE,
    guess_max = 5000,
    skip_empty_rows = TRUE,
    show_col_types = FALSE,
    lazy = FALSE
  )

  # User overrides
  if (!is.null(spec))
    args <- utils::modifyList(args, val = spec, keep.null = TRUE)
  if (!missing(...))
    args <- utils::modifyList(args, val = list(...), keep.null = TRUE)

  # Ensure col_types are always present
  if (is.null(args$col_types))
    args$col_types <- readr::cols()

  # Provided args
  args$file <- file
  args$col_names <- col_names
  args$na = unique(c("", ".", na_geno))

  if (is.null(spec)) {
    cli::cli_progress_step("Scanning VCF fields")
    args$col_types <- do.call(readr::spec_delim, args)
    cli::cli_progress_done()
  }

  if (length(args$col_types$cols) > 0 && !all(col_names %in% names(args$col_types$cols)))
    warning("The provided 'col_names' do not match with 'spec$col_types'.")

  for (current in skip_cols)
    args$col_types$cols[[current]] <- readr::col_skip()

  args
}

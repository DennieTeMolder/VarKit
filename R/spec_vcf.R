##' @export
spec_vcf <- function(file, col_names = NULL, skip_cols = NULL, na_geno = c("./.", ".|."), spec = NULL, ...) {
  if (!is.null(col_names)) {
    stopifnot(is.character(col_names))
  } else if (!is.null(spec) && !is.null(spec$col_names)) {
    col_names <- spec$col_names
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

  # Provided args
  args$file <- file
  args$col_names <- col_names
  args$na <- unique(c("", ".", na_geno))

  if (is.null(spec))
    args$col_types <- .quick_spec_delim(args)

  if (length(args$col_types$cols) > 0 && !all(names(args$col_types$cols) %in% col_names))
    warning("Not all 'spec$col_types' are in 'col_names'.")

  for (current in skip_cols)
    args$col_types$cols[[current]] <- readr::col_skip()

  args
}

# HACK readr::spec* always loads the entire file despite 'guess_max', here we force it
.quick_spec_delim <- function(args) {
  args$file <- I(readr::read_lines(args$file, skip = find_header_line(args$file), n_max = args$guess_max))
  do.call(readr::spec_delim, args)
}

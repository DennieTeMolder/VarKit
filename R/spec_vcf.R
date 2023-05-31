##' @export
spec_vcf <- function(file, col_names, skip_cols = NULL, na_geno = "./.", spec = NULL, ...) {
  if (!is.null(skip_cols)) {
    stopifnot(is.character(skip_cols))
    if (any(c("chrom", "pos") %in% skip_cols))
      stop("The 'chrom' and/or 'pos' columns cannot be skipped!")
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

  if (!is.null(spec))
    args <- utils::modifyList(args, val = spec, keep.null = TRUE)
  if (!missing(...))
    args <- utils::modifyList(args, val = list(...), keep.null = TRUE)

  # Provided args
  args$file <- file
  args$col_names <- col_names
  args$na = unique(c("", ".", na_geno))

  spec <- do.call(readr::spec_delim, args)

  if (!all(skip_cols %in% names(spec$cols)))
    stop("Not all column names provided for 'skip_cols' are in the VCF header!")
  for (current in skip_cols)
    spec$cols[[current]] <- readr::col_skip()

  args$col_types <- spec
  args$col_names <- names(spec$cols)
  args
}

##' Load A VCF File From Disk
##'
##' @title Load A VCF File From Disk
##' @param file String, path to the vcf(.bgz) file to load
##' @param col_select Columns to include in the results. You can use the same mini-language as ‘dplyr::select()’ to refer to the columns by name. Use ‘c()’ to use more than one selection expression.
##' @param header varkit_header object, used to determine the column names of 'file'.
##' @param n_max Integer, maximum number of variants to load. Zero or negative will load all.
##' @param skip Integer, number of variants to skip before loading 'n_max' variants.
##' @param na_geno Character vector, exact string(s) to convert to NA in the genotype fields
##' @return A VarKit object
##' @export
##' @author Dennie te Molder
##' @examples
##' file <- system.file("extdata", "celegans.vcf.bgz", package = "varkit")
##' vcf <- read_vcf(file)
read_vcf <- function(file,
                     col_select = NULL,
                     header = NULL,
                     n_max = -1,
                     skip = 0,
                     na_geno = c("./.", ".|.")
                     ) {
  if (!is.null(header))
    validate_varkit_header(header)

  # Open the file if needed
  if (inherits(file, "connection")) {
    if (!is_open.connection(file))
      stop("The provided 'file' connection is invalid!")
  } else {
    file <- file(file, "r")
    on.exit(close(file))
  }

  # Handle the VCF header, it will be skipped by "comment.char" if the header is provided
  if (is.null(header)) {
    if (isSeekable(file) && seek.connection(file) != 0)
      stop("A header should be supplied if connection 'file' is not at the start!")

    cli::cli_progress_step("Reading VCF header")
    header <- read_vcf_header(file)

    if (length(header$col_names) < 3)
      stop("At line ", length(meta(header)) + 1, ": Invalid VCF, the header has fewer then three tab-separated fields!")
  }

  cli::cli_progress_step("Reading VCF body")
  body <- read_tbl(file,
    col_select = {{ col_select }},
    col_names = header$col_names,
    nrows = n_max,
    skip = skip,
    na.strings = unique(c("", ".", na_geno)),
    col_types = c(chrom = "c", pos = "i", id = "c", ref = "c", alt = "c",
                  qual = "n", filter = "c", info = "c", format = "c"),
    sep = "\t",
    header = FALSE,
    check.names = FALSE,
    quote = "",
    as.is = TRUE,
    fill = FALSE,
    strip.white = TRUE,
    comment.char = "#"
  )
  cli::cli_progress_done()

  if (nrow(body) < 1)
    return(NULL)

  cli::cli_progress_step("Collecting results")

  # Detect fixed fields
  fixed_idx <- match(.fixed_columns(), colnames(body))
  fixed_idx <- fixed_idx[!is.na(fixed_idx)]

  if (length(fixed_idx) == 0)
    stop("No fixed columns detected in VCF, ensure they are lower case in col_names(header)!")
  if (ncol(body) == length(fixed_idx))
    stop("Only fixed columns detected in VCF, missing genotypes!")
  if (!all(c("chrom", "pos") %in% colnames(body)[fixed_idx]))
    stop("Missing 'chrom' and/or 'pos' columns in VCF!")

  # Move fixed fields to a separate df
  fxd <- body[, fixed_idx]
  body <- body[, -fixed_idx]

  # The remaining fields are genotype calls
  body <- varkit_geno(body)

  varkit(site = fxd, geno = body, meta = meta(header))
}

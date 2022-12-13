##' Load A VCF File From Disk
##'
##' @title Load A VCF File From Disk
##' @param file String, path to the vcf(.bgz) file to load
##' @param header Varkit_header
##' @param skip_cols Character vector, columns to skip loading. Should match names in `read_vcf_header(file)$col_names`.
##' @param na_geno Character vector, exact string(s) to convert to NA in the genotype fields
##' @param header_max_lines Integer, max number of lines to check for the header
##' @param ... Arguments passed to [readr::read_tsv()]
##' @return A VarKit object
##' @export
##' @author Dennie te Molder
##' @examples
##' file <- system.file("extdata", "celegans.vcf.bgz", package = "varkit")
##' vcf <- read_vcf(file, skip_cols = c("info", "filter", "N2"))
read_vcf <- function(file,
                     header = NULL,
                     skip_cols = NULL,
                     na_geno = "./.",
                     header_max_lines = 1e5,
                     ...) {
  # Argument checks
  if (!is.null(header))
    stopifnot(is_varkit_header(header))
  if (!is.null(skip_cols))
    stopifnot(is.character(skip_cols))
  stopifnot(is.character(na_geno))

  if (is.null(header))
    header <- read_vcf_header(file, n_max = header_max_lines)

  # Header checks
  if (length(header$col_names) < 2)
    stop("The number of columns is too low, make sure the file is TAB delimited!")
  if (!all(skip_cols %in% header$col_names))
    stop("Not all column names provided for 'skip_cols' are in the VCF header!")

  # Create a string of column types (see readr::cols())
  types <- rep("?", length(header$col_names))
  types[header$col_names %in% skip_cols] <- "_"
  types[header$col_names %in% c("chrom", "ref", "alt", "format")] <- "c"
  types[header$col_names == "pos"] <- "d"
  types <- paste(types, collapse = "")

  data <- readr::read_tsv(file,
    col_names = header$col_names,
    col_types = types,
    comment = "#",
    na = c(".", na_geno),
    ...
  )

  varkit(data, meta = header$meta)
}

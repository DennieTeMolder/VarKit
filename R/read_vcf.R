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
    if (!is_open(file))
      stop("The provided 'file' connection is invalid!")
    con <- file
  } else {
    con <- file(file, "r")
    on.exit(close(con))
  }

  # Handle the VCF header
  if (isSeekable(con) && seek(con) != 0) {
    if (is.null(header))
      stop("A header should be supplied if connection 'file' is not at the start!")
  } else if (!is.null(header)) {
    skip <- skip + find_header_line(summary(con)$description)
  } else {
    header <- read_vcf_header(con)
    if (length(header$col_names) < 3)
      stop("At line ", length(header$meta) + 1, ": Invalid VCF, the header has fewer then three tab-separated fields!")
  }

  # Read the VCF body
  body <- read_tbl(con,
    col_select = {{ col_select }},
    col_names = col_names(header),
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

  if (nrow(body) < 1)
    return(NULL)

  varkit(body, meta = meta(header))
}

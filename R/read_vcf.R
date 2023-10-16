##' Load A VCF File From Disk
##'
##' @title Load A VCF File From Disk
##' @param file String, path to the vcf(.bgz) file to load
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
read_vcf <- function(file, header = NULL, n_max = -1, skip = 0, na_geno = c("./.", ".|.")) {
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

  # Handle the header
  if (seek(con) != 0) {
    if (is.null(header))
      stop("A header should be supplied if connection 'file' is not at the start!")
  } else if (!is.null(header)) {
    skip <- skip + find_header_line(summary(con)$description)
  } else {
    header <- read_vcf_header(con)
    if (length(header$col_names) < 3)
      stop("At line ", length(header$meta) + 1, ": Invalid VCF, the header has fewer then three tab-separated fields!")
  }

  body <- .read_vcf_body(con, col_names = header$col_names, n_max = n_max, skip = skip, na_geno = na_geno)
  if (nrow(body) < 1)
    stop("The VCF body appears to be empty!")

  varkit(body, meta = header$meta)
}

.read_vcf_body <- function(con, col_names, n_max = -1, skip = 0, na_geno = c("./.", ".|.")) {
  if (!inherits(con, "connection"))
    stop("'con' should be a file connection!")

  na_geno <- unique(c("", ".", na_geno))

  tibble::as_tibble(utils::read.delim(
    con,
    col.names = col_names,
    nrow = n_max,
    skip = skip,
    na.strings = na_geno,
    header = FALSE,
    check.names = FALSE,
    quote = "",
    as.is = TRUE,
    fill = FALSE,
    strip.white = TRUE,
    comment.char = "#"
  ))
}

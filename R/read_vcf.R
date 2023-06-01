##' Load A VCF File From Disk
##'
##' @title Load A VCF File From Disk
##' @param file String, path to the vcf(.bgz) file to load
##' @param header Varkit_header
##' @param skip_cols Character vector, columns to skip loading. Should match names in `read_vcf_header(file)$col_names`.
##' @param na_geno Character vector, exact string(s) to convert to NA in the genotype fields
##' @param spec A list of args passed to [readr::read_delim()]
##' @param ... Arguments passed to [readr::read_delim()], overriding 'spec'
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
                     spec = NULL,
                     ...) {
  if (!is.null(header)) {
    stopifnot(is_varkit_header(header))
  } else {
    cli::cli_progress_step("Parsing VCF header")
    header <- read_vcf_header(file)
    cli::cli_progress_done()
  }

  # Checks
  if (length(header$col_names) < 3)
    stop("The number of columns is too low, make sure the file is TAB delimited!")

  # Create spec that prompts spec_vcf() to skip guessing col_types (2x quicker)
  if (is.null(spec))
    spec <- list(col_types = NULL)

  data <- do.call(
    readr::read_delim,
    spec_vcf(file, col_names = header$col_names, skip_cols = skip_cols, na_geno = na_geno, spec = spec, ...)
  )

  varkit(data, meta = header$meta)
}

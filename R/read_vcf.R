##' Load A VCF File From Disk
##'
##' Load a VCF file into R as a 'varkit' object.
##' @title Load A VCF File From Disk
##' @param path String, path to the vcf(.bgz) file to load
##' @param ploidy Integer, ploidy number of the organism in the VCF
##' @param skip_cols Character vector, columns to skip loading. Should match names in `read_vcf_header(path)$col_names`.
##' @param na Character vector, what strings to recognise as NA when reading
##' @param ... Arguments passed to [readr::read_tsv()]
##' @return VarKit object
##' @author Dennie te Molder
##' @export
##' @examples
##' path <- system.file("extdata", "celegans.vcf.bgz", package = "varkit")
##' vcf <- read_vcf(path, ploidy = 2, skip_cols = c("info", "filter", "N2"))
read_vcf <- function(path,
                     ploidy = 2,
                     skip_cols = NULL,
                     na = "auto-generate",
                     ...) {
  # Argument checks
  stopifnot(is.numeric(ploidy))
  if (!is.null(skip_cols))
    stopifnot(is.character(skip_cols))
  stopifnot(is.character(na))

  header <- read_vcf_header(path)

  # Header checks
  if (length(header$col_names) < 2)
    stop("The number of columns is too low, make sure the file is TAB delimited!")
  if (!all(skip_cols %in% header$col_names))
    stop("Not all column names provided for 'skip_cols' are in the VCF header!")

  # Create a string of column types (see readr::cols())
  types <- rep("?", length(header$col_names))
  types[header$col_names %in% skip_cols] <- "_"
  types[header$col_names == "chrom"] <- "c"
  types[header$col_names == "pos"] <- "d"
  types <- paste(types, collapse = "")

  # NAs are represented differently after the format field
  # TODO This only works if GT is the only format field
  if (na == "auto-generate")
    na <- c(".", paste(rep(".", ploidy), collapse = "/"))

  data <- readr::read_tsv(path,
    col_names = header$col_names,
    col_types = types,
    comment = "#",
    na = na,
    ...
  )

  varkit(data, ploidy = ploidy, meta = header$meta)
}

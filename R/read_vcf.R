##' Load A VCF File From Disk
##'
##' .. content for \details{} ..
##' @title Load A VCF File From Disk
##' @param path String, path of vcf(.bgz) file to load
##' @return A VarKit object
##' @author Dennie te Molder
##' @export
##' @examples
##' path <- system.file("extdata", "celegans.vcf.bgz", package = "varkit")
##' vcf <- read_vcf(path)

read_vcf <- function(path,
                     ploidy = 2,
                     skip_cols = c("info", "filter")) {
  # Checks
  if (!is.null(skip_cols))
    stopifnot(is.character(skip_cols))

  header <- read_vcf_header(path)

  if (!all(skip_cols %in% header$col_names))
    warning("Not all columns provided for 'skip_cols' are in the VCF header!")

  # Create a string of column types (see readr::cols())
  types <- rep("?", length(header$col_names))
  types[header$col_names %in% skip_cols] <- "_"
  types[header$col_names == "chrom"] <- "c"
  types[header$col_names == "pos"] <- "d"
  types <- paste(types, collapse = "")

  # NAs are represented differently after the format field
  # TODO This only works if GT is the only format field
  na <- c(".", paste(rep(".", ploidy), collapse = "/"))

  data <- readr::read_tsv(path,
    col_names = header$col_names,
    col_types = types,
    comment = "#",
    na = na
  )

  varkit(data, header)
}

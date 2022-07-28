##' Load A VCF File From Disk
##'
##' .. content for \details{} ..
##' @title Load A VCF File From Disk
##' @param path String, path of vcf(.bgz) file to load
##' @return A VarKit object
##' @author Dennie te Molder
##' @export
##' @examples
##' path <- "./celegans.vcf.bgz"
##' vcf <- read_vcf(path)

read_vcf <- function(path, keep_info = FALSE, keep_filter = FALSE) {
  header <- read_vcf_header(path)

  # Load the entire file according to user specifications
  spec <- readr::cols(chrom = "c", pos = "d", info = "_", filter = "_")
  if (keep_info && "info" %in% col_names)
    spec$cols$info <- NULL
  if (keep_filter && "filter" %in% col_names)
    spec$cols$filter <- NULL

  geno <- readr::read_tsv(path,
    col_names = header$col_names,
    col_types = spec,
    comment = "#",
    na = "."
  )

  varkit(geno, header = header$header, spec = header$spec)
}

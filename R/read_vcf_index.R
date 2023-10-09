##' @export
read_vcf_index <- function(vcf_path, validate = TRUE) {
  stopifnot(file.exists(vcf_path))

  index_path <- paste0(vcf_path, ".index.tsv")

  origin <- readr::read_lines(index_path, n_max = 1)
  if (!stringr::str_starts(origin, "#varkit_header_sha1="))
    stop("The first line of the index is missing the 'varkit_header_sha1' attribute!")
  origin <- stringr::str_sub(origin, start = 21)

  index <- readr::read_tsv(index_path, skip = 1, col_types = readr::cols(id = "c", linum = "i", n_lines = "i"))

  header <- read_vcf_header(vcf_path)
  if (validate && origin != digest::sha1(header))
    stop("The VCF seems to have changed since it has been indexed!")

  spec <- spec_vcf(vcf_path, col_names = header$col_names)
  varkit_index(index, file = vcf_path, header = header, spec = spec)
}

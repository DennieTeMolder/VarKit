##' @export
get_vcf_index <- function(vcf_path, size_bp = -1, chunk_size = 1e4, validate = TRUE, ...) {
  index_path <- get_index_file(vcf_path)

  index <- NULL
  if (file.exists(index_path)) {
    if (!validate || .has_matching_index(vcf_path)) {
      index <- read_vcf_index(vcf_path, validate = FALSE, ...)
    } else {
      .alert_info("The VCF index is out dated, creating a new one.")
    }
  }

  if (is.null(index)) {
    index <- index_vcf(vcf_path, size_bp = size_bp, chunk_size = chunk_size, ...)
    write_vcf_index(index, vcf_path = vcf_path)
  }

  index
}

##' @export
write_vcf_index <- function(index, vcf_path) {
  UseMethod("write_vcf_index", index)
}

##' @export
write_vcf_index.varkit_index <- function(index, vcf_path) {
  stopifnot(c("id", "linum", "n_lines") %in% colnames(index))
  stopifnot(file.exists(vcf_path))

  index_path <- paste0(vcf_path, ".index.tsv")

  header <- read_vcf_header(vcf_path)
  sha <- digest::sha1(header)

  cli::cli_progress_step("Writing VCF index to: {index_path}")
  readr::write_lines(paste0("#varkit_header_sha1=", sha), file = index_path)
  readr::write_tsv(index, file = index_path, append = TRUE, col_names = TRUE)
}

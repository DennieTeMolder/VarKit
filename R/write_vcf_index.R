##' @export
write_vcf_index <- function(index, vcf_path) {
  UseMethod("write_vcf_index", index)
}

##' @export
write_vcf_index.varkit_index <- function(index, vcf_path) {
  validate_varkit_index(index)

  header <- read_vcf_header(vcf_path)
  expected_start <- nrow(meta(header)) + 2
  if (index$linum[1] != expected_start)
    stop("The first value of 'linum' does not match with the VCF header size! Expected value: ", expected_start)
  sha <- digest::sha1(header)

  index_path <- get_index_file(vcf_path)
  .msg_step("Writing VCF index to: {index_path}")
  con <- file_con(index_path, "w")
  on.exit(close(con))
  write_lines2(paste0("#varkit_header_sha1=", sha), file = con)
  write_tbl(index, file = con, append = TRUE, col_names = TRUE)
}

##' @export
get_index_file <- function(vcf_path) {
  stopifnot(file.exists(vcf_path) && !dir.exists(vcf_path))
  paste0(vcf_path, ".index.tsv")
}

##' @export
write_vcf_index <- function(index, vcf_path) {
  UseMethod("write_vcf_index", index)
}

##' @export
write_vcf_index.varkit_index <- function(index, vcf_path) {
  validate_varkit_index(index)

  header <- read_vcf_header(vcf_path)
  expected_start <- nrow(meta(header)) + 2L
  if (index$linum[1L] != expected_start)
    stop("The first value of 'linum' does not match with the VCF header size! Expected value: ", expected_start)

  index_path <- get_index_file(vcf_path)
  .cli_step("Writing VCF index to: {index_path}")

  con <- file_con(index_path, "w")
  .defer(close(con))

  write_lines2(paste0("#varkit_header_hash=", varkit_hash(header)), file = con)
  write_tbl(index, file = con, append = TRUE, col_names = TRUE)
}

##' @export
get_index_file <- function(vcf_path) {
  if (!file.exists(vcf_path))
    stop("VCF file does not exist: ", vcf_path)
  if (endsWith(vcf_path, ".index.tsv"))
    stop("Expected a VCF path, but got a path to an index!")
  paste0(vcf_path, ".index.tsv")
}

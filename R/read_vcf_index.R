##' @export
read_vcf_index <- function(vcf_path, validate = TRUE, ...) {
  stopifnot(file.exists(vcf_path))

  index_path <- get_index_file(vcf_path)
  cli::cli_progress_step("Reading VCF index: {index_path}")

  if (!file.exists(index_path))
    stop("The provided 'vcf_path' has no associated index!")

  origin <- readLines(index_path, n = 1)
  if (!stringr::str_starts(origin, "#varkit_header_sha1="))
    stop("The first line of the index is missing the 'varkit_header_sha1' attribute!")
  origin <- stringr::str_sub(origin, start = 21)

  index <- read_tbl(index_path, skip = 1, colClasses = c(id = "c", linum = "i", n_lines = "i"))

  if (validate && origin != digest::sha1(read_vcf_header(vcf_path)))
    stop("The VCF seems to have changed since it has been indexed!")

  varkit_index(index, file = vcf_path, ...)
}

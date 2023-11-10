##' @export
read_vcf_index <- function(vcf_path, validate = TRUE, ...) {
  stopifnot(file.exists(vcf_path))

  index_path <- get_index_file(vcf_path)
  cli::cli_progress_step("Reading VCF index: {index_path}")

  if (!file.exists(index_path))
    stop("The provided 'vcf_path' has no associated index!")
  if (validate && !.has_matching_index(vcf_path))
    stop("The VCF seems to have changed since it has been indexed!")

  index <- read_tbl(
    index_path,
    colClasses = c(id = "c", linum = "i", n_lines = "i"),
    comment.char = "#",
    guess.max = -1L
  )

  varkit_index(index, file = vcf_path, ...)
}

.has_matching_index <- function(path) {
  index_path <- get_index_file(path)

  origin <- readLines(index_path, n = 1L)
  if (!stringr::str_starts(origin, "#varkit_header_sha1="))
    stop("The first line of the index is missing the 'varkit_header_sha1' attribute!")
  origin <- stringr::str_sub(origin, start = 21L)

  origin == digest::sha1(read_vcf_header(path))
}

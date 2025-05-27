##' @export
index_vcf_header <- function(file, chunk_size = 1e4, ...) {
  index <- tibble::tibble(
    id = "chunk_size",
    linum = find_header_line(file) + 1L,
    n_lines = chunk_size
  )
  varkit_index(index = index, file = file, ...)
}

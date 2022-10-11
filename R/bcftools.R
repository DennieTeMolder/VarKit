##' @export
write_bcftools_regions <- function(x, file) {
  UseMethod("write_bcftools_regions", x)
}

##' @export
write_bcftools_regions.varkit <- function(x, file) {
  readr::write_tsv(fixed(x, only_positions = TRUE), file = file, col_names = FALSE)
}

##' @export
write_bcftools_samples <- function(x, file) {
  UseMethod("write_bcftools_samples", x)
}

##' @export
write_bcftools_samples.varkit <- function(x, file) {
  readr::write_lines(samples(x), file = file)
}

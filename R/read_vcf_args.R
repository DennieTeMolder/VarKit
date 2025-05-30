##' @export
read_vcf_args <- function(index) {
  stopifnot(is_varkit_index(index))
  get("read_vcf_args", envir = .env(index), inherits = FALSE)
}

`read_vcf_args<-` <- function(index, value) {
  stopifnot(is_varkit_index(index))
  assign("read_vcf_args", value = value, envir = .env(index))
  index
}

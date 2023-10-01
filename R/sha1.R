##' @exportS3Method digest::sha1
sha1.varkit <- function(x, ...) {
  digest::sha1(list(x$use_site, samples.varkit(x)))
}

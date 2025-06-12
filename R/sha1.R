##' @exportS3Method digest::sha1
sha1.varkit <- function(x, ...) {
  digest::sha1(list(
    .drm_if_else(x, node = "sites"),
    samples.varkit(x)
  ))
}

##' @exportS3Method digest::sha1
sha1.varkit_header <- function(x, ...) {
  digest::sha1(list(meta(x), samples.varkit_header(x)))
}

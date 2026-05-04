##' @exportS3Method digest::sha1
sha1.varkit <- function(x, ...) {
  validate_varkit(x)
  all_sites <- rd_unclass(x)$site
  digest::sha1(list(
    all_sites[intersect(names(all_sites), c("chrom", "pos", "ref", "alt"))],
    samples.varkit(x)
  ))
}

##' @exportS3Method digest::sha1
sha1.varkit_header <- function(x, ...) {
  validate_varkit_header(x)
  digest::sha1(list(meta(x), samples.varkit_header(x)))
}

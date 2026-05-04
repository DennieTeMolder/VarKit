##' @export
varkit_hash <- function(origin) {
  if (!is_varkit(origin))
    return(rlang::hash(origin))
  rlang::hash(list(
    samples.varkit(origin),
    site.varkit(origin, "chrom", "pos")
  ))
}

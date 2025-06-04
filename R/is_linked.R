##' @export
is_linked <- function(x, ...) {
  UseMethod("is_linked", x)
}

##' @export
is_linked.varkit_linkage_graph <- function(x, vcf, ...) {
  rlang::check_dots_used()
  stopifnot(is_varkit(vcf))
  .ensure_igraph()

  if (is.null(get_origin(x))) {
    stop("The linkage graph 'x' does not have a 'varkit_origin' attribute!\n",
         "The linkage graph is most likely corrupted.", call. = FALSE)
  } else if (!originates_from(x, origin = vcf)) {
    stop("The provided linkage graph does not originate from 'vcf'!\n",
         "Likely either the mask or samples(vcf) differs between the two.", call. = FALSE)
  }

  deg <- igraph::degree(x, mode = "in", loops = FALSE)
  if (any(deg >= 2))
    stop("The linkage graph appears to be corrupted!", call. = FALSE)

  #
  is_linked <- rep(FALSE, n_sites.varkit(vcf))
  is_linked[as.integer(names(which(deg == 1)))] <- TRUE
  is_linked
}

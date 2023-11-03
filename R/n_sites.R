##' @export
n_sites <- function(x, ...) {
  UseMethod("n_sites", x)
}

##' @export
n_sites.varkit <- function(x, ..., mask = TRUE) {
  if (mask)
    return(sum(x$use_site))
  length(x$use_site)
}

##' @export
n_sites.varkit_index <- function(x, ...) {
  sum(x[["n_lines"]])
}

##' @export
n_sites.varkit_geno <- function(x, ...) {
  ncol(x)
}

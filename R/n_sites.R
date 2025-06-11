##' @export
n_sites <- function(x, ...) {
  UseMethod("n_sites", x)
}

##' @export
n_sites.varkit <- function(x, ..., mask = TRUE) {
  if (mask) {
    drm_n_selected(drm_get(x$site), "sites")
  } else {
    drm_n_total(drm_get(x$site), "sites")
  }
}

##' @export
n_sites.varkit_index <- function(x, ...) {
  sum(x[["n_lines"]])
}

##' @export
n_sites.varkit_geno <- function(x, ...) {
  ncol(x)
}

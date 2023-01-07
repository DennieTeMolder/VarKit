##' @export
to_tibble <- function(x, ...) {
  UseMethod("to_tibble", x)
}

##' @export
to_tibble.varkit_geno <- function(x, ...) {
  tibble::as_tibble(as.data.frame(t(x), ...))
}

##' @export
to_tibble.varkit <- function(x, ...) {
  dplyr::bind_cols(site(x), to_tibble(geno(x)))
}

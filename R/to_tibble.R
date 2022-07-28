##' @export
to_tibble <- function(x) {
  UseMethod("to_tibble", x)
}

##' @export
to_tibble.varkit_geno <- function(x) {
  x <- as.data.frame(t(x))
  tibble::as_tibble(x)
}
